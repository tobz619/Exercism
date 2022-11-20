{-# LANGUAGE RecordWildCards   #-}

module RunParser where

import Control.Monad
import qualified Control.Applicative
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme = L.lexeme ws

integerParser :: Parser Integer
integerParser = undefined

something = parseTest (satisfy (== 'a') :: Parser Char) ""

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

data Scheme = SchemeData | SchemeFile | SchemeFtp | SchemeHttps
            | SchemeHttp | SchemeIrc | SchemeMailto deriving (Show, Eq, Enum)

pSchemeF :: Parser Text
pSchemeF =  string "data" <|> string "file" <|> string "ftp" <|>
           string "http" <|> string "https" <|> string "irc" <|>
           string "mailto"

pSchemeC :: Parser Text
pSchemeC = choice $ string <$> ["data","file","ftp","http","https","irc","mailto"]

getScheme :: Parser Scheme
getScheme = choice $ zipWith (<$) [SchemeData .. SchemeMailto] ["data","file","ftp","https","http","irc","mailto"]

data Uri = Uri { uriScheme :: Scheme
               , uriAuthority :: Maybe Authority
               , uriPath :: Text
               , uriQuery :: Maybe Query
               , uriFragment :: Maybe Fragment
               }
               deriving (Show, Eq)

data Authority = Authority { authUser :: Maybe (Text, Text)
                           , authHost :: Text
                           , authPort :: Maybe Int }
                           deriving (Show, Eq)

-- | Parses a scheme and immediately checks for a comma.
-- >>> parseTest pUri "irc"
-- 1:4:
--   |
-- 1 | irc
--   |    ^
-- unexpected end of input
-- expecting ':'
-- >>> parseTest pUri "irc:"
-- Uri {uriScheme = "irc"}
pUri :: Parser Uri
pUri = (Uri <$> getScheme <* char ':'
           <*> getUriAuthA
           <*> (getSlash *> getPath)
           <*> getQuery
           <*> getFragment) <*eof

getUriAuth :: Parser (Maybe Authority)
getUriAuth = optional $ do 
    void (string "//")
    authUser <- optional . try $ do              -- (2)
      user <- T.pack <$> some alphaNumChar       -- (3)
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal) -- (4)
    return (Authority authUser authHost authPort)    -- (5)

getUriAuthA :: Parser (Maybe Authority)
getUriAuthA = optional . try $ Authority <$> user <*> host <*> port
            where user = string "//" *> (optional . try $ do
                            user <- T.pack <$> some alphaNumChar
                            _ <- char ':'
                            password <- T.pack <$> some alphaNumChar
                            _ <- char '@'
                            return (user, password))
                  host = T.pack <$> some (alphaNumChar <|> char '.')
                  port = optional (char ':' *> L.decimal) 

getSlash :: Parser (Maybe Char)
getSlash = optional $ char '/'

getPath :: Parser Text
getPath =  T.pack <$> many alphaNumChar

newtype Query = Query Text deriving (Show, Eq)

getQuery :: Parser (Maybe Query)
getQuery = optional $ Query <$> (T.pack <$> (char '?' *> many alphaNumChar))

newtype Fragment = Fragment Text deriving (Show, Eq)

getFragment :: Parser (Maybe Fragment)
getFragment = optional $ Fragment <$> (T.pack <$> (char '#' *> many alphaNumChar))

