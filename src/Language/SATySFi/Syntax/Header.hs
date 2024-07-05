{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SATySFi.Syntax.Header (
  Module (..),
  HeaderDecl (..),
  parseHeaders,
) where

import Control.Applicative
import Data.Bifunctor qualified as Bi
import Data.Char qualified as C
import Data.Functor (void)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Void (Void)
import Development.Shake.Classes (Binary, Hashable, NFData)
import GHC.Generics
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

newtype Module = Module {moduleName :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Binary, Hashable, NFData)

data HeaderDecl = Import Module | Require Module
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type Parser = P.Parsec Void T.Text

spaces :: Parser ()
spaces = L.space P.space1 (L.skipLineComment "%") (fail "No block comment")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaces

reserved :: T.Text -> Parser ()
reserved n = void $ symbol n <* P.notFollowedBy P.alphaNumChar

parseHeaders :: T.Text -> Either String [HeaderDecl]
parseHeaders = Bi.first P.errorBundlePretty . P.parse (spaces *> P.many headerP <* P.takeRest) "Input"

headerP :: Parser HeaderDecl
headerP = importP <|> requireP

importP :: Parser HeaderDecl
importP =
  P.try $
    Import
      <$ symbol "@"
      <* reserved "import"
      <* symbol ":"
      <*> moduleP

requireP :: Parser HeaderDecl
requireP =
  P.try $
    Require <$ symbol "@" <* reserved "require" <* symbol ":" <*> moduleP

moduleP :: Parser Module
moduleP = lexeme $ Module <$> P.takeWhile1P (Just "module name") (\c -> c /= '%' && not (C.isSpace c))
