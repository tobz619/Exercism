{-# OPTIONS -Wunused-imports #-}

module WordProblem (answer) where

import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

answer :: String -> Maybe Integer
answer problem = undefined

whatIsParser :: Parser Text
whatIsParser = string' "What is " :: Parser Text

ws :: Parser ()
ws = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme = L.lexeme ws

integerParser :: Parser Integer
integerParser = undefined

something = parseTest (satisfy (== 'a') :: Parser Char) ""