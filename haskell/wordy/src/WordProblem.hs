module WordProblem (answer) where

import Text.Megaparsec
import Text.Megapparsec.Char
import Data.Void
import Data.Text (Text)

type Parser = Parsec Void String

answer :: String -> Maybe Integer
answer problem = error "You need to implement this function."

whatIsParser :: Parser Char
whatIsParser = undefined

something = parseTest (satisfy (== 'a') :: Parser Char) "bvc"