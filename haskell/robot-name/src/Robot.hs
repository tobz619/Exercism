module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State
import System.Random
import Data.List

newtype Robot = Robot String deriving (Show, Eq)
type RunState = [Robot]

initialState :: RunState
initialState = []

mkRobot :: StateT RunState IO Robot
mkRobot = StateT f where f acc =  do name <- liftIO genName
                                     if Robot name `notElem` acc
                                        then return (Robot name, Robot name:acc)
                                        else runStateT mkRobot acc


genName :: IO String
genName = (++) <$> liftIO (replicateM 2 (getStdRandom (randomR ('A','Z'))))
               <*> liftIO (replicateM 3 (getStdRandom (randomR ('0','9'))))

resetName :: Robot -> StateT RunState IO ()
resetName robot = do robots <- get
                     newName <- liftIO $ evalStateT mkRobot robots
                     modify (robot `delete`)
                     modify (newName :)

robotName :: Robot -> IO String
robotName (Robot s) = pure s
