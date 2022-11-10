{-# LANGUAGE RecordWildCards   #-}

module WordProblem (answer) where

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
answer problem = error "You need to implement this function."

whatIsParser :: Parser Char
whatIsParser = undefined

parseA = parseTest (satisfy (== 'a') :: Parser Char) "a"

parseChar x = \inp -> parseTest (char x :: Parser Char) inp

newline = single '\n' :: Parser Char

mySequenceA :: Parser (Char, Char, Char)
mySequenceA = (,,) <$> char 'a' <*> char 'b' <*> char 'c'

mySequenceM :: Parser (Char, Char, Char)
mySequenceM = do a <- char 'a'
                 b <- char 'b'
                 c <- char 'c'
                 return (a,b,c)

manyAs :: Parser String
manyAs = many (char 'a') :: Parser String

manyChain :: Char -> Parser String
manyChain x = many (char x) :: Parser String

stopMany :: Char -> Parser String
stopMany x = many (char x) <* eof

-- From now on we will be developing a real, useful parser that can parse URIs of the following form:
--
-- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

pScheme :: Parser Text
pScheme =  string "data" <|> string "file" <|> string "ftp" <|>
           string "http" <|> string "https" <|> string "irc" <|>
           string "mailto"

pSchemeC :: Parser Text
pSchemeC = choice $ string <$> ["data","file","ftp","http","https","irc","mailto"]

data Uri = Uri {uriScheme :: Text} deriving (Show, Eq)

pUri :: Parser Uri
pUri = do r <- pScheme
          _ <- char ':'
          return (Uri r )
