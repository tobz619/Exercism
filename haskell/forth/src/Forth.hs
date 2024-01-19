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
import Control.Monad.State ( MonadState(get, put), State, gets, modify' )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))
import Text.Parsec ( char, digit, letter, spaces, many, sepBy, eof, alphaNum)
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import qualified Data.Text as T
import Text.Parsec.Char (space)
import Data.Maybe (fromMaybe)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data Operation = PLUS | MINUS | MULT | DIV
  deriving (Show, Eq)

data ForthState = ForthState { stack :: !Stack, env :: !(Map Text (Evaluator ())) }
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


opHandler :: Operation -> Evaluator ()
opHandler PLUS = do x <- pop; y <- pop; push (x+y)
opHandler MINUS = do x <- pop; y <- pop; push (x-y)
opHandler MULT = do x <- pop; y <- pop; push (x*y)
opHandler DIV = do x <- pop; y <- pop; if y == 0 then throwError DivisionByZero else push (x `div` y)



stackOps :: Map Text (Evaluator ())
stackOps = Map.fromList $
          [ ("drop", drp)
          , ("dup", dup)
          , ("swap", swap)
          , ("over", over)
          , ("+", opHandler PLUS)
          , ("-", opHandler MINUS)
          , ("*", opHandler MULT)
          , ("/", opHandler DIV)
          ]



lookupSt :: Text -> Evaluator ()
lookupSt op = do fs <- get
                 let ops = env fs
                 fromMaybe (throwError $ UnknownWord op) (Map.lookup op ops)

dup, drp, swap, over :: Evaluator ()
dup = do sta <- gets stack
         case nonEmpty sta of
          Nothing -> throwError StackUnderflow
          Just (x:| xs) -> modify' (\s -> s {stack = x : x : xs }) 

drp = do sta <- gets stack
         case nonEmpty sta of
          Nothing -> throwError StackUnderflow
          Just (_:| xs) -> modify' (\s -> s {stack = xs })

swap = do sta <- gets stack
          case nonEmpty sta of
           Nothing -> throwError StackUnderflow
           Just (x:| (y:xs)) -> modify' (\s -> s {stack = y : x : xs })
           Just _ -> throwError StackUnderflow

over = do sta <- gets stack
          case nonEmpty sta of
           Nothing -> throwError StackUnderflow
           Just (x:| (y : xs)) -> modify' (\s -> s {stack = y : x : y : xs })
           Just _ -> throwError StackUnderflow

parseAssignment :: Parser (Text, [Text])
parseAssignment = do _ <- spaces *> char ':' <* spaces
                     name <- pack <$> many letter
                     _ <- spaces
                     def <- parseInp 
                     _ <- spaces *> char ';'
                     return (name, def)

parseInp :: Parser [Text]
parseInp = ((pack <$> many alphaNum) `sepBy` space) <* eof