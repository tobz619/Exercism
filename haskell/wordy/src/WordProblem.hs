{-# LANGUAGE RecordWildCards   #-}

module WordProblem where

import Control.Monad
import qualified Control.Applicative
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

answer :: String -> Maybe Integer
answer problem = undefined

whatIsParser :: Parser Text
whatIsParser = string' "What is " :: Parser Text