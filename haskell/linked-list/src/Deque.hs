module Deque where

import Data.IORef
import Control.Monad.IO.Class (MonadIO(liftIO))

{- Using Haskell lists feels cheap: going to steal this FingerTree implementation from 
   https://www.youtube.com/watch?v=-HZ4bo_USvE 
-}


type Deque a = ([a],[a])

mkDeque :: IO (IORef (Deque a))
mkDeque = newIORef ([], []) 

pop :: Show a => IORef (Deque a) -> IO (Maybe a)
pop ref = do q <- readIORef ref
             process q
                  where process (_,[]) = return Nothing
                        process (_,xs) = do writeIORef ref (reverse (tail xs), tail xs)
                                            readIORef ref >>= print
                                            return $ Just . head $ xs

push :: Show a => IORef (Deque a) -> a -> IO ()
push ref x = do q <- readIORef ref
                process q
                  where process (_,xs) = do writeIORef ref (reverse (x:xs), x:xs)
                                            readIORef ref >>= print

unshift :: Show a => IORef (Deque a) -> a -> IO ()
unshift ref x = do q <- readIORef ref
                   process q
                  where process (xs,_) = do writeIORef ref (x:xs, reverse (x:xs))
                                            readIORef ref >>= print

shift :: Show a => IORef (Deque a) -> IO (Maybe a)
shift ref = do q <- readIORef ref
               process q
                  where process ([],_) = return Nothing
                        process (xs,_) = do writeIORef ref (tail xs, reverse (tail xs))
                                            readIORef ref >>= print
                                            return $ Just . head $ xs

test = do
      deque <- mkDeque
      push deque 'a'
      push deque 'b'
      pop deque >>= print
      pop deque >>= print

test2 = do
      deque <- mkDeque
      push deque 'a'
      push deque 'b'
      pop deque
      push deque 'c'
      shift deque
      unshift deque 'd'
      push deque 'e'
      shift deque
      pop deque
      pop deque