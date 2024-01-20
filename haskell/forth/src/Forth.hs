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
import Control.Monad.State ( MonadState(get, put), State, gets, modify', execState, runState )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))
import Text.Parsec ( char, digit, letter, spaces, many, sepBy, eof, alphaNum, parse, oneOf, (<|>), many1, manyTill, sepBy1)
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
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

data ForthState = ForthState { stack :: !Stack, env :: Map Text (Evaluator ()) }
type Stack = [Int]

type Evaluator = ExceptT ForthError (State ForthState)

emptyState :: ForthState
emptyState = ForthState [] stackOps


-- >>> toList <$> evalText ": dup-twice dup dup ;" emptyState
-- Right []
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text fs = let hasErr = runExceptT (textToEval text)
                    in case runState hasErr fs of
                        (Left err, _) -> Left err
                        (_, s) -> Right s


toList :: ForthState -> [Int]
toList = reverse . stack

push :: Int -> Evaluator ()
push x = do fs <- get
            let newStack = x : stack fs
            modify' (\s -> s {stack = newStack})

pop :: Evaluator Int
pop = do fs <- get
         case nonEmpty . stack $ fs of
            Nothing -> throwError StackUnderflow
            Just (x :| xs) -> do modify' (\s -> s { stack = xs })
                                 return x

opHandler :: Operation -> Evaluator ()
opHandler PLUS = do x <- pop; y <- pop; push (x+y)
opHandler MINUS = do x <- pop; y <- pop; push (y-x)
opHandler MULT = do x <- pop; y <- pop; push (x*y)
opHandler DIV = do x <- pop; y <- pop; if x == 0 then throwError DivisionByZero else push (y `div` x)


stackOps :: Map Text (Evaluator ())
stackOps = Map.fromList
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
lookupSt !op = do fs <- get
                  let ops = env fs
                  fromMaybe (throwError $ UnknownWord op) (Map.lookup op ops)

addCommand :: Text -> [Text] -> Evaluator ()
addCommand name def = do curEnv <- gets env
                         case Map.lookup name curEnv of
                          
                          Nothing -> let newEnv = Map.insert name (evalInp def) curEnv 
                                      in modify' (\s -> s {env = newEnv})
                          
                          Just f -> undefined

redefineCommand name oldCommand newCommand currentEnv = let tempEnv = Map.insert "temp" oldCommand currentEnv
                                                            tempEnv' = Map.alter (const $ Just newCommand) name tempEnv
                                                            tempEnvState = do st <- modify' (\s -> s {env = tempEnv'} ) >> get
                                                                              undefined
                                                            
                                                         in tempEnv

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
           Just (x:| (y : xs)) -> do modify' (\s -> s {stack = y : x : y : xs })
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


evalInp :: [Text] -> Evaluator ()
evalInp = mapM_ eval
      where eval x =
              case readMaybe . T.unpack $ x of
                Just v -> push v
                Nothing -> lookupSt x

textToEval :: Text -> Evaluator ()
textToEval text = case parse parseRegular "" text of
                    Right x -> evalInp x
                    Left _ ->
                      case parse parseAssignment "" text of
                        Right (n, d) -> addCommand n d
                        Left _ -> throwError InvalidWord
