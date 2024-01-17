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
import Control.Monad.State 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))
import Text.Parsec ( char, digit, letter, spaces, many, Parsec )
import Text.Parsec.Text (Parser)
import Data.Void (Void)
import Text.Read (readMaybe)
import Control.Exception.Base

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState { stack :: Stack, env :: Map Text Op }
type Stack = [Int]
type Op = Stack -> Either ForthError Stack

emptyState :: ForthState
emptyState = ForthState [] stackOps

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text stack = error "You need to implement this function."

toList :: ForthState -> [Int]
toList = stack


push :: Int -> State ForthState ()
push x = do fs <- get
            let newStack = x : stack fs
            put $ fs {stack = newStack}

pop :: State ForthState (Either ForthError Int)
pop = do fs <- get
         let list = nonEmpty . stack $ fs 
         case list of
            Nothing -> return $ Left StackUnderflow
            Just (x :| xs) -> do put $ fs { stack = xs }
                                 return $ Right x


applyBinOp :: (Int -> Int -> r) -> State ForthState (Either ForthError r)
applyBinOp op = do x <- pop
                   y <- pop
                   let !v = liftM2 op x y
                   return v


binops :: Map Char (Int -> Int -> Int)
binops = Map.fromList $ 
        [ ('+', (+) )
         , ('-', (-) )
         , ('/', div )
         , ('*', (*) )
         ]


stackOps :: Map Text (Stack -> Either ForthError Stack)
stackOps = Map.fromList $ 
          [ ("drop", drp)
          , ("dup", dup)
          , ("swap", swap)
          , ("over", over)
          ]

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

parseAssignment :: Parser (Text, Text)
parseAssignment = do _ <- spaces *> char ':' <* spaces
                     name <- pack <$> many letter
                     _ <- spaces
                     def <- pack <$> many letter
                     _ <- spaces *> char ';'
                     return (name, def)

command :: Parser Text
command = spaces *> (pack <$> many letter) <* spaces

num :: Parser (Either ForthError Int)
num = do _ <- spaces
         mint <- many digit
         return $ maybe 
          (Left $ UnknownWord (pack mint)) 
          Right 
          (readMaybe . show $ mint :: Maybe Int)

divZero DivideByZero = Left DivisionByZero
divZero _ = undefined
