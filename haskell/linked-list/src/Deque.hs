module Deque where

import Data.IORef

data Deque a = End | Node { prev :: Deque a, current :: a, next :: Deque a}
    deriving (Eq)


instance Show a => Show (Deque a) where
    show n@(Node End _ _) = processing n
        where processing End = "]"
              processing (Node End x ne) = "Current deque: [" ++ show x ++ processing ne
              processing (Node _ x End) = " <> " ++ show x ++ "]"
              processing (Node _ x ne) = show x ++ " <> " ++ processing ne
    
    show n = show (prev n)

mkDeque :: IO (IORef (Deque a))
mkDeque = newIORef End

pop :: Show a => IORef (Deque a) -> IO (Maybe a)
pop ref = do queue <- readIORef ref
             process queue
            where process n@(Node End x End) = do writeIORef ref End
                                                  putStrLn $ "Popping "++ show x
                                                  putStrLn "Empty Deque"
                                                  return $ Just x
                  process n@(Node pre x End) = do writeIORef ref $ Node End (current pre) (next pre)
                                                  putStrLn $ "Popping "++ show x
                                                  print $ Node End (current pre) (next pre)
                                                  return $ Just x
                  process End = return Nothing
                  process n = process $ next n




push :: Show a => IORef (Deque a) -> a -> IO ()
push ref x = do putStrLn $ "Putting " ++ show x ++ " at end of deque"
                queue <- readIORef ref
                putLast queue
                readIORef ref >>= print
                where putLast End = writeIORef ref (Node End x End)
                      putLast node@(Node _ _ End) = let newNode = Node node x End
                                                        putNode = node {next = newNode}
                                                     in writeIORef ref putNode
                      putLast n = putLast (next n)



unshift :: Show a => IORef (Deque a) -> a -> IO ()
unshift ref x = do queue <- readIORef ref
                   putStrLn $ "Putting " ++ show x ++ " at start of the queue"
                   writeIORef ref (putStart queue)
                   readIORef ref >>= print
                                 where putStart n@(Node End _ ne) =
                                        let newNode = Node End x newN
                                            newN = n {prev = newNode}
                                         in newNode
                                       putStart End = Node End x End
                                       putStart n = putStart (prev n)
                   



shift :: Show a => IORef (Deque a) -> IO (Maybe a)
shift ref = do queue <- readIORef ref
               putStrLn $ "Popping the head of the queue and shifting"
               process queue
               where process End = return Nothing
                     process n@(Node End x End) = do writeIORef ref End
                                                     return $ Just x
                     process (Node End x ne) = do writeIORef ref $ Node End (current ne) (next ne)
                                                  print $ Node End (current ne) (next ne)
                                                  return $ Just x
                     process n = process (prev n)


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