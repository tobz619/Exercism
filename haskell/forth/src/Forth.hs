{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text, pack)
import Control.Monad.State.Strict ( MonadState(get), gets, modify', StateT (runStateT), execStateT, evalStateT )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))
import Text.Parsec ( char, letter, spaces, eof, alphaNum, parse, oneOf, (<|>), many1, manyTill, sepBy1)
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Text.Parsec.Char (space)
import Control.Monad (foldM)
import Control.Monad.Except (MonadError(throwError) )
import Data.Maybe (fromMaybe)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data Operation = PLUS | MINUS | MULT | DIV
  deriving (Show, Eq)

data ForthState = ForthState { stack :: !Stack, env :: Map Text [Text] }
type Stack = [Int]

type EnvEval = ForthState -> Either ForthError ForthState

emptyState :: ForthState
emptyState = ForthState [] defaultEnv

evalText :: Text -> EnvEval
evalText text fs = case parse parseRegular "" (T.toLower text) of
                      Right ts -> mkEval fs ts
                      Left _ -> case parse parseAssignment "" (T.toLower text) of

                        Left _ -> Left InvalidWord
                        Right (name, def) -> let newEnv = handleCommand name def (env fs)
                                              in Right $ fs {env = newEnv}

toList :: ForthState -> [Int]
toList = reverse . stack

defaultEnv :: Map Text [Text]
defaultEnv = Map.fromList
          [ ("drop", ["drop"])
          , ("dup", ["dup"])
          , ("swap", ["swap"])
          , ("over", ["over"])
          , ("+", ["+"])
          , ("-", ["-"])
          , ("*", ["*"])
          , ("/", ["/"])
          ]

push :: Int -> StateT ForthState (Either ForthError) ForthState
push x = do curStack <- gets stack
            let newStack = x : curStack
            modify' ( \s -> s {stack = newStack} )
            get

pop :: StateT ForthState (Either ForthError) Int
pop = do curStack <- gets stack
         case nonEmpty curStack of
            Nothing -> throwError StackUnderflow
            Just (x :| xs) -> do modify' (\s -> s { stack = xs })
                                 return x


opHandler :: Operation -> EnvEval
opHandler PLUS = evalStateT $ do x <- pop; y <- pop; push (x+y)
opHandler MINUS = evalStateT $ do x <- pop; y <- pop; push (y-x)
opHandler MULT = evalStateT $ do x <- pop; y <- pop; push (x*y)
opHandler DIV = evalStateT $ do x <- pop; y <- pop; if x == 0 then throwError DivisionByZero else push (y `div` x)

evalInp :: Text -> ForthState -> Either ForthError ForthState
evalInp "+" = opHandler PLUS
evalInp "-" = opHandler MINUS
evalInp "*" = opHandler MULT
evalInp "/" = opHandler DIV
evalInp "drop" = drp
evalInp "dup" = dup
evalInp "swap" = swap
evalInp "over" = over
evalInp x = case readMaybe . T.unpack $ x :: Maybe Int of
              Just i -> execStateT (push i)
              _ -> \fs -> maybe (Left $ UnknownWord x) (mkEval fs) (Map.lookup x (env fs))

handleCommand :: Text -> [Text] -> Map Text [Text] -> Map Text [Text]
handleCommand name newDef curEnv = Map.insert name body curEnv
              where body = concatMap (`evalWord` curEnv) newDef


evalWord :: Ord k => k -> Map k [k] -> [k]
evalWord w envi = fromMaybe [w] (Map.lookup w envi)

mkEval :: ForthState -> [Text] -> Either ForthError ForthState
mkEval fs = foldM stateMaker fs
        where stateMaker state text = let commands = evalWord text (env fs)
                                       in foldM (flip evalInp) state commands 

dup, drp, swap, over :: EnvEval
dup = evalStateT $
      do sta <- gets stack
         case nonEmpty sta of
          Nothing -> throwError StackUnderflow
          Just (x:| _) -> push x

drp = evalStateT $
      do sta <- gets stack
         case nonEmpty sta of
          Nothing -> throwError StackUnderflow
          Just _ -> pop >> get

swap = evalStateT $
       do sta <- gets stack
          case nonEmpty sta of
           Nothing -> throwError StackUnderflow
           Just (x:| (y:xs)) -> modify' (\s -> s {stack = y : x : xs }) >> get
           Just _ -> throwError StackUnderflow

over = evalStateT $
       do sta <- gets stack
          case nonEmpty sta of
           Nothing -> throwError StackUnderflow
           Just (x:| (y : xs)) -> do modify' (\s -> s {stack = y : x : y : xs }) >> get
           Just _ -> throwError StackUnderflow


parseAssignment :: Parser (Text, [Text])
parseAssignment = do _ <- spaces *> char ':' <* spaces
                     name <- pack <$> many1 (letter <|> oneOf "+*/-")
                     def <- spaces *> manyTill parseInp (char ';' <* eof)
                     return (name, def)

parseInp :: Parser Text
parseInp = pack <$> (spaces *> many1 (alphaNum <|> oneOf "+/*-") <* spaces)

parseRegular :: Parser [Text]
parseRegular = fmap pack <$> (many1 (alphaNum <|> oneOf "+/*-") `sepBy1` space)
