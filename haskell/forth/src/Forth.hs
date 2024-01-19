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
import Control.Monad.State ( MonadState(get, put), State )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))
import Text.Parsec ( char, digit, letter, spaces, many, sepBy, eof, alphaNum)
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import qualified Data.Text as T
import Text.Parsec.Char (space)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data Operation = PLUS | MINUS | MULT | DIV
  deriving (Show, Eq)

data ForthState = ForthState { stack :: !Stack, env :: !(Map Text Op) }
type Stack = [Int]
type Op = Stack -> Either ForthError Stack

type Evaluator = ExceptT ForthError (State ForthState)

emptyState :: ForthState
emptyState = ForthState [] stackOps

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text fs = error "You need to implement this function."

toList :: ForthState -> [Int]
toList = stack


push :: Int -> Evaluator ()
push x = do fs <- get
            let newStack = x : stack fs
            put $ fs {stack = newStack}

pop :: Evaluator Int
pop = do fs <- get
         let list = nonEmpty . stack $ fs
         case list of
            Nothing -> throwError StackUnderflow
            Just (x :| xs) -> do put $ fs { stack = xs }
                                 return x


applyBinOp :: (Int -> Int -> Either ForthError Int) -> Evaluator ()
applyBinOp op = do x <- pop
                   y <- pop
                   case op x y of
                    Right res -> do push res
                    Left err -> throwError err

applyStackOp :: (Stack -> Either ForthError Stack) -> Evaluator ()
applyStackOp op = do fs <- get
                     let sta = stack fs 
                     case op sta of
                      Left err -> throwError err
                      Right st -> put $ fs {stack = st}

runCommand :: Text -> Evaluator ()
runCommand text = do fs <- get
                     let ops = env fs
                     case Map.lookup (T.toLower text) ops of
                      Nothing -> throwError (UnknownWord text)
                      Just x -> applyStackOp x

opHandler :: Integral a => Operation -> a -> a -> Either ForthError a
opHandler PLUS x y = return (x + y)
opHandler MINUS x y = return (x - y)
opHandler DIV _ 0 = Left DivisionByZero
opHandler DIV x y = return (x `div` y)
opHandler MULT x y= return (x * y)

binops :: Map Char (Int -> Int -> Either ForthError Int)
binops = Map.fromList $
        [ ('+', opHandler PLUS )
        , ('-', opHandler MINUS )
        , ('/', opHandler DIV )
        , ('*', opHandler MULT)
        ]

stackOps :: Map Text (Stack -> Either ForthError Stack)
stackOps = Map.fromList $
          [ ("drop", drp)
          , ("dup", dup)
          , ("swap", swap)
          , ("over", over)
          ]


lookupSt :: Text -> Evaluator Op
lookupSt op = do fs <- get
                 let ops = env fs
                 maybe (throwError $ UnknownWord op) return (Map.lookup op ops)

dup, drp, swap, over :: Stack -> Either ForthError Stack
dup [] = Left StackUnderflow
dup (x:xs) = Right (x:x:xs)

drp [] = Left StackUnderflow
drp (x:xs) = Right xs

swap [] = Left StackUnderflow
swap [x] = Left StackUnderflow
swap(x:y:xs) = Right (y:x:xs)

over [] = Left StackUnderflow
over [x] = Left StackUnderflow
over (x:y:ys) = Right (y:x:y:ys)

parseAssignment :: Parser (Text, [Text])
parseAssignment = do _ <- spaces *> char ':' <* spaces
                     name <- pack <$> many letter
                     _ <- spaces
                     def <- parseInp 
                     _ <- spaces *> char ';'
                     return (name, def)

parseInp :: Parser [Text]
parseInp = ((pack <$> many alphaNum) `sepBy` space) <* eof