module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State
import System.Random
import Control.Concurrent.MVar
import Data.List

type Robot = MVar String
type RunState = [String]

initialState :: RunState
initialState = []

mkRobot :: StateT RunState IO Robot
mkRobot = StateT f where f acc = do name <- genName
                                    if name `elem` acc
                                       then runStateT mkRobot acc
                                       else do newName <- liftIO $ newMVar name
                                               return (newName, name:acc)
                                  


genName :: IO String
genName = (++) <$> liftIO (replicateM 2 (getStdRandom (randomR ('A','Z'))))
               <*> liftIO (replicateM 3 (getStdRandom (randomR ('0','9'))))

resetName :: Robot -> StateT RunState IO ()
resetName robot = do robo <- liftIO $ readMVar robot
                     newName <- liftIO genName
                     acc <- get
                     if newName `elem` acc
                       then resetName robot
                       else do inita <- liftIO $ swapMVar robot newName
                               modify (delete robo)
                               modify (inita:)


robotName :: Robot -> IO String
robotName = readMVar      
