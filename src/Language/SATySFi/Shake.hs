{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.SATySFi.Shake (
  defaultMain,
  defaultMainWith,
  optionsP,
  Options,
  satysfiRules,
) where

import Control.Applicative ((<**>))
import Control.Exception (Exception (..))
import Data.ByteString qualified as BS
import Data.Functor (void, (<&>))
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath qualified as Shake
import GHC.Generics
import GHC.Stack (HasCallStack)
import Language.SATySFi.Syntax.Header (HeaderDecl (..), Module (..), parseHeaders)
import Options.Applicative qualified as Opts
import Path
import Path qualified as P
import Path.IO (getHomeDir, makeAbsolute, resolveFile')
import Paths_shake_satysfi (version)

data Options = Options
  { targets :: !(NonEmpty FilePath)
  , baseDir :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, Binary)

defaultMain :: IO ()
defaultMain = do
  opts <- Opts.customExecParser (Opts.prefs Opts.subparserInline) optionsP
  defaultMainWith opts

optionsP :: Opts.ParserInfo Options
optionsP =
  Opts.info
    (parser <**> Opts.helper)
    (Opts.fullDesc <> Opts.progDesc "Build SATySFi project")
  where
    parser = do
      targets <- NE.fromList <$> Opts.some (Opts.strArgument (Opts.metavar "TARGET" <> Opts.help "Target SATySFi document to build"))
      baseDir <-
        Opts.optional $
          Opts.strOption $
            Opts.short 'o'
              <> Opts.long "outdir"
              <> Opts.metavar "DIR"
              <> Opts.help "the directory to put the output files in"

      pure Options {..}

defaultMainWith :: (HasCallStack) => Options -> IO ()
defaultMainWith opts = shakeArgs shakeOptions {shakeChange = ChangeDigest, shakeVersion = showVersion version} $ do
  satysfiRules
  let targets =
        opts.targets <&> \targ0 ->
          (fromMaybe <$> id <*> L.stripPrefix "./") $
            case Shake.takeExtension targ0 of
              ".pdf" -> targ0
              ".saty" -> targ0 Shake.-<.> "pdf"
              _ -> targ0 Shake.<.> "pdf"
  liftIO $ putStrLn $ "Needing: " <> show (NE.toList targets)
  want $ NE.toList targets

deriving anyclass instance Binary (Path r b)

data ModCmd
  = Req (Path Abs File)
  | Imp (Path Abs File)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, Binary)

newtype Hash = Hash Int
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, Binary)

type instance RuleResult ModCmd = Hash

readFileText' :: (HasCallStack) => Path r File -> Action T.Text
readFileText' fp = do
  need [toFilePath fp]
  liftIO . fmap TE.decodeUtf8 . BS.readFile . toFilePath $ fp

satysfiRules :: (HasCallStack) => Rules ()
satysfiRules = do
  alternatives do
    "_build/*.pdf" %> \out -> do
      putInfo $ "Genearting: " <> out
      relFP <- either (error . displayException) pure $ parseRelFile out
      absFP <-
        makeAbsolute
          =<< either
            (error . displayException)
            pure
            ( P.replaceExtension ".saty" $
                parent (parent relFP) </> filename relFP
            )
      putInfo $ "Resolved: " <> fromAbsFile absFP
      src <- readFileText' absFP
      putInfo "Parsing headers..."
      hdrs <- either error pure . parseHeaders $ src
      putInfo "Traversing dependencies"
      void $ askOracles =<< mapM (resolveCmd (parent absFP)) hdrs
      cmd_ "satysfi" "-o" out (fromAbsFile absFP)
    "//*.pdf" %> \out -> do
      putInfo $ "Genearting: " <> out
      let fp = out Shake.-<.> ".saty"
      absFP <- resolveFile' fp
      putInfo $ "Resolved: " <> fromAbsFile absFP
      src <- readFileText' absFP
      putInfo "Parsing headers..."
      hdrs <- either error pure . parseHeaders $ src
      putInfo "Traversing dependencies"
      void $ askOracles =<< mapM (resolveCmd $ parent absFP) hdrs
      cmd_ "satysfi" fp

  void $ addOracle \case
    Req fp -> do
      putInfo $ "Requiring: " <> fromAbsFile fp
      src <- readFileText' fp
      hdrs <- either error pure . parseHeaders $ src
      void $ askOracles =<< mapM (resolveCmd (parent fp)) hdrs
      pure $ hashIt src
    Imp pkg -> do
      putInfo $ "Importing: " <> fromAbsFile pkg
      src <- readFileText' pkg
      hdrs <- either error pure . parseHeaders $ src
      void $ askOracles =<< mapM (resolveCmd (parent pkg)) hdrs
      pure $ hashIt src

hashIt :: (Hashable a) => a -> Hash
hashIt = Hash . hash

resolveCmd :: Path Abs Dir -> HeaderDecl -> Action ModCmd
resolveCmd d (Require m) = Req <$> findPackage d m.moduleName
resolveCmd d (Import m) = Imp <$> findLocalPackage d m.moduleName

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (!x : xs) = do
  b <- p x
  if b then pure (Just x) else findM p xs

findPackage :: Path Abs Dir -> T.Text -> Action (Path Abs File)
findPackage cwd targ = do
  pkgDirs <- liftIO $ getPackageDirs cwd
  findPackage' pkgDirs targ

findLocalPackage :: Path Abs Dir -> T.Text -> Action (Path Abs File)
findLocalPackage cwd = findPackage' (NE.singleton cwd)

findPackage' :: (HasCallStack) => NonEmpty (Path Abs Dir) -> T.Text -> Action (Path Abs File)
findPackage' (sd :| rest) targ = do
  candidates <-
    if Shake.isAbsolute $ T.unpack targ
      then do
        let satyh = T.unpack targ Shake.<.> ".satyh"
            satyg = T.unpack targ Shake.<.> ".satyg"
        pure [satyh, satyg]
      else do
        let satyh = T.unpack targ Shake.<.> "satyh"
            satyg = T.unpack targ Shake.<.> "satyg"
        pure
          $ concatMap
            (\d -> [fromAbsDir d Shake.</> satyh, fromAbsDir d Shake.</> satyg])
          $ sd : rest
  mtarg <- findM doesFileExist candidates
  maybe
    ( error $
        unlines $
          "Package not found: " <> T.unpack targ
            : "Candidates:"
            : map ("- " <>) candidates
    )
    (liftIO . resolveFile')
    mtarg

getPackageDirs :: Path Abs Dir -> IO (NonEmpty (Path Abs Dir))
getPackageDirs cwd = do
  home <- getHomeDir
  let homeSaty = home </> [reldir|.satysfi|]
      cwdSaty = cwd </> [reldir|.satysfi|]
      usrLocalSaty = [absdir|/usr/local/satysfi|]
      usrShareSaty = [absdir|/usr/share/satysfi|]
  pure $
    NE.fromList
      [ root </> locDist </> [reldir|packages|]
      | locDist <- [[reldir|local|], [reldir|dist|]]
      , root <- [cwdSaty, homeSaty, usrLocalSaty, usrShareSaty]
      ]
