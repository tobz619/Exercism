module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Control.Monad.IO.Class (MonadIO(liftIO))

data Deque a = Nil | Node { prev :: Deque a, current :: a, next :: Deque a}

mkDeque :: IO (IORef (Deque a))
mkDeque = newIORef Nil 

pop :: IORef (Deque a) -> IO (Maybe a)
pop deque = do q <- readIORef deque 
               let (res, newq) = popper q
               writeIORef deque newq
               return res

push :: IORef (Deque a) -> a -> IO ()
push deque x = modifyIORef deque (pusher x)

unshift :: IORef (Deque a) -> a -> IO ()
unshift deque x = modifyIORef deque (unshifter x)

shift :: IORef (Deque a) -> IO (Maybe a)
shift deque = do q <- readIORef deque 
                 let (res, newq) = popper q
                 writeIORef deque newq
                 return res


test = do
      deque <- mkDeque
      push deque 'a' >>= print
      push deque 'b' >>= print
      pop deque >>= print
      pop deque >>= print

popper :: Deque a -> (Maybe a, Deque a)
popper Nil              = (Nothing, Nil)
popper (Node Nil x Nil) = (Just x, Nil)
popper (Node p x Nil) = let newNode = Node (prev p) (current p) Nil
                         in (Just x, newNode)
popper (Node _ _ ne) = popper ne

pusher :: a -> Deque a -> Deque a
pusher x Nil                 = Node Nil x Nil
pusher x last@(Node p c Nil) = let newLast = Node p c newNode
                                   newNode = Node newLast x Nil
                                in newNode
pusher x n = pusher x (next n)

unshifter :: a -> Deque a -> Deque a
unshifter x Nil                   = Node Nil x Nil
unshifter x first@(Node Nil c ne) = let newFirst = Node Nil x newNext
                                        newNext = Node newFirst c ne
                                     in newNext
unshifter x n  = unshifter x (prev n)