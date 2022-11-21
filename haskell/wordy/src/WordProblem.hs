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
whatIsParser = string' "What is "

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal


ops :: [(a , Parser b)] -> Parser a
ops xs = foldr1 (<|>) op
        where op = do (op, p) <- xs
                      return $ do _ <- lexeme p
                                  return op



addops :: Parser (Integer -> Integer -> Integer)
addops = ops [ ((+) , string' "plus")
             , ((-) , string' "minus") ]