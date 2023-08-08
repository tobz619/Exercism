module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Deque a = End | Node { prev :: Deque a, current :: a, next :: Deque a} 
    deriving (Show, Eq)

mkDeque :: IO (IORef (Deque a))
mkDeque = newIORef End

pop :: IORef (Deque a) -> IO (Maybe a)
pop ref = do queue <- readIORef ref
             case queue of
                End -> pure Nothing
                nodes -> getLast nodes
                    where getLast (Node _ x End) = do modifyIORef ref removeLast
                                                      return $ Just x
                          getLast n = getLast (next n)
                          removeLast node@(Node p x End) = Node (prev p) (current p) End
                          removeLast n = removeLast (next n)


push :: IORef (Deque a) -> a -> IO ()
push ref x = do queue <- readIORef ref 
                case queue of
                 End -> writeIORef ref (Node End x End)
                 nodes -> modifyIORef ref modLast
                    where modLast n@(Node p c End) = let newNode = Node n x End
                                                      in Node p c newNode
                          modLast n = modLast (next n)
                          
                    

unshift :: IORef (Deque a) -> a -> IO ()
unshift deque x = error "You need to implement this function."

shift :: IORef (Deque a) -> IO (Maybe a)
shift deque = error "You need to implement this function."

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
      pop deque >>= print
      push deque 'c'
      shift deque >>= print
      unshift deque 'd'
      push deque 'e'
      shift deque >>= print
      pop deque >>= print
      pop deque >>= print