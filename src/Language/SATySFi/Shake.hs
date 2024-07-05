{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath qualified as Shake
import GHC.Generics
import GHC.Stack (HasCallStack)
import Language.SATySFi.Syntax.Header (HeaderDecl (..), Module (..), parseHeaders)
import Options.Applicative qualified as Opts
import Path
import Path.IO (getHomeDir, resolveFile')

newtype Options = Options {target :: FilePath}
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
    parser = Options <$> Opts.strArgument (Opts.metavar "TARGET" <> Opts.help "Target SATySFi document to build")

defaultMainWith :: (HasCallStack) => Options -> IO ()
defaultMainWith opts = shakeArgs shakeOptions {shakeChange = ChangeDigest} $ do
  satysfiRules
  let target = case Shake.takeExtension opts.target of
        ".pdf" -> opts.target
        ".saty" -> opts.target Shake.-<.> "pdf"
        _ -> opts.target Shake.<.> "pdf"
  liftIO $ putStrLn $ "Needing: " <> target
  want [target]

data SearchIn = SearchIn (Path Abs Dir) HeaderDecl
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, Binary)

deriving anyclass instance Binary (Path Abs Dir)

type instance RuleResult SearchIn = ()

readFileText' :: (HasCallStack) => Path r File -> Action T.Text
readFileText' fp = do
  need [toFilePath fp]
  liftIO . T.readFile . toFilePath $ fp

satysfiRules :: (HasCallStack) => Rules ()
satysfiRules = do
  "//*.pdf" %> \out -> do
    putInfo $ "Genearting: " <> out
    let fp = out Shake.-<.> ".saty"
    absFP <- resolveFile' fp
    putInfo $ "Resolved: " <> fromAbsFile absFP
    src <- readFileText' absFP
    putInfo "Parsing headers..."
    hdrs <- either error pure . parseHeaders $ src
    putInfo "Traversing dependencies"
    void $ askOracles $ SearchIn (parent absFP) <$> hdrs
    cmd_ "satysfi" fp

  void $ addOracle \case
    SearchIn cwd (Require fp) -> do
      putInfo $ "Requiring: " <> T.unpack fp.moduleName
      pkg <- findPackage cwd fp.moduleName
      src <- readFileText' pkg
      hdrs <- either error pure . parseHeaders $ src
      void $ askOracles $ SearchIn (parent pkg) <$> hdrs
    SearchIn cwd (Import fp) -> do
      putInfo $ "Importing: " <> T.unpack fp.moduleName
      pkg <- findLocalPackage cwd fp.moduleName
      src <- readFileText' pkg
      hdrs <- either error pure . parseHeaders $ src
      void $ askOracles $ SearchIn (parent pkg) <$> hdrs

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
