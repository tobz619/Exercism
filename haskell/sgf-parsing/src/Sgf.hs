{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Sgf (parseSgf) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree, unfoldTree, Tree(Node))
import Text.Parsec.Text (Parser)
import Text.Parsec (between, many, (<|>), try, optionMaybe, parseTest, parse, many1, (<?>), upper, tab, crlf, newline, string, anyChar)
import Text.Parsec.Char ( alphaNum, char, noneOf, oneOf )


type SGFParser = Parser

type SgfTree = Tree SgfNode

type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe SgfTree
parseSgf sgf =
    case parse treeParser "" sgf of
                Left _ -> Nothing
                Right vs -> Just vs


testInp1 :: Text
testInp1 = "(;A[\\]b\nc\\\nd\t\te\\\\ \\\n\\]])"

testInp2 :: Text
testInp2 = "(;FF[4](;B[aa];W[ab])(;B[dd];W[ee]))"

testInp3 :: Text
testInp3 = "(;A[hello\\\tworld])"

treeParser :: SGFParser SgfTree
treeParser = brackets variationParser

variationParser :: SGFParser SgfTree
variationParser =  do name <- nodeParser
                      subt <- many (try variationParser <|> brackets variationParser)
                      return $ Node name subt

nodeParser :: SGFParser SgfNode
nodeParser = do _ <- char ';'
                kvs <- many keyValParser
                return $ Map.fromList kvs


keyValParser :: SGFParser (Text, [Text])
keyValParser = do key <- T.pack <$> many1 upper
                  vals <- many1 valueParser
                  return (key, vals) <?> "Key val parser"

valueParser :: SGFParser Text
valueParser = do T.pack <$> between (char '[') (char ']') (many values)


brackets :: SGFParser a -> SGFParser a
brackets = between (char '(') (char ')')

values = try escapeChars <|> noneOf "]"

whiteSpaceP = do _ <- tab <|> crlf
                 return ' '

escapeNewLine = newline >> return '\n'

escapedNewLine = do _ <- escapeNewLine
                    values                  

escapeChars = escapeNewLine <|> whiteSpaceP <|> (char '\\' >> (whiteSpaceP <|> try escapedNewLine <|> anyChar))