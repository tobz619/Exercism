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
import Control.Monad.State.Strict ( MonadState(get, put), State, gets, modify', execState, evalState, StateT (runStateT), execStateT, evalStateT )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))
import Text.Parsec ( char, digit, letter, spaces, many, sepBy, eof, alphaNum, parse, oneOf, (<|>), many1, manyTill, sepBy1)
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, liftEither, Except, runExcept)
import qualified Data.Text as T
import Text.Parsec.Char (space)
import Control.Monad (foldM)
import Data.List (foldl')
import Data.Either (fromRight)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data Operation = PLUS | MINUS | MULT | DIV
  deriving (Show, Eq)

data ForthState = ForthState { stack :: !Stack, env :: Map Text EnvEval }
type Stack = [Int]

type EnvEval = ForthState -> Either ForthError ForthState

emptyState :: ForthState
emptyState = ForthState [] defaultEnv


-- >>> toList <$> evalText ": dup-twice dup dup ;" emptyState
-- Right []
--
-- >>> toList <$> (evalText "3 5 7" emptyState >>= evalText "+")
-- Right [3,12]
evalText :: Text -> EnvEval
evalText text fs = case parse parseAssignment "" text of
                    Right (name, d) -> assignCommand name d fs
                    
                    Left _ -> case parse parseRegular "" text of
                                Left _ -> Left InvalidWord
                                Right d -> mkState fs d


toList :: ForthState -> [Int]
toList = reverse . stack

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

dup, drp, swap, over :: EnvEval
dup = evalStateT $
      do sta <- gets stack
         case nonEmpty sta of
          Nothing -> throwError StackUnderflow
          Just (x:| xs) -> do modify' (\s -> s {stack = x : x : xs })
                              get

drp = evalStateT $
      do sta <- gets stack
         case nonEmpty sta of
          Nothing -> throwError StackUnderflow
          Just (_:| xs) -> do modify' (\s -> s {stack = xs })
                              get

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


defaultEnv :: Map Text EnvEval
defaultEnv = Map.fromList
          [ ("drop", drp)
          , ("dup", dup)
          , ("swap", swap)
          , ("over", over)
          , ("+", opHandler PLUS)
          , ("-", opHandler MINUS)
          , ("*", opHandler MULT)
          , ("/", opHandler DIV)
          ]

parseAssignment :: Parser (Text, [Text])
parseAssignment = do _ <- spaces *> char ':' <* spaces
                     name <- pack <$> many1 (letter <|> oneOf "+*/-")
                     def <- spaces *> manyTill parseInp (char ';' <* eof)
                     return (name, def)

parseInp :: Parser Text
parseInp = pack <$> (spaces *> many1 (alphaNum <|> oneOf "+/*-") <* spaces)


-- >>> parse parseRegular "" "+ - +"
-- Right ["+","-","+"]
parseRegular :: Parser [Text]
parseRegular = fmap pack <$> (many1 (alphaNum <|> oneOf "+/*-") `sepBy1` space)


applyForthFunc :: Text -> ForthState -> Either ForthError ForthState
applyForthFunc name fs = case Map.lookup name curEnv of
                          Nothing -> Left $ UnknownWord name
                          Just f -> f fs
                      where curEnv = env fs

evalInp :: Text -> EnvEval
evalInp t fs = case readMaybe . T.unpack $ t :: Maybe Int of
              Just i ->  evalStateT (push i) fs
              Nothing -> applyForthFunc t fs


assignCommand :: Text -> [Text] -> EnvEval
assignCommand name newDef fs = case Map.lookup name (env fs) of
                                    Nothing -> let newEnv = Map.insert name (`mkState` newDef) (env fs)
                                                in return $ fs {env = newEnv}
                                    
                                    Just f ->  let newEnv = Map.insert name (`mkState` newDef) (fromRight (env fs) (env <$> f fs))
                                                in return $ fs {env = newEnv}

mkState :: ForthState -> [Text] -> Either ForthError ForthState
mkState fs = foldl' stateMaker (pure fs)
        where stateMaker state text = state >>= \s -> evalInp text s


-- >>> runTexts ["1 2 3 4", "+"]
-- Right [1,2,7]

-- >>> runTexts [": foo dup ;", ": foo dup dup ;", "1 foo"]
-- Right [1,1,1]

-- >>> runTexts [ ": foo 5 ;" , ": bar foo ;" , ": foo 6 ;", "bar foo" ] -- => Right [5,6]
-- Right [6,6]
runTexts :: [Text] -> Either ForthError [Int]
runTexts = fmap toList . foldM (flip evalText) emptyState
