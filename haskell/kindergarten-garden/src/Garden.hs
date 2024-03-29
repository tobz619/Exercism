module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Maybe ( fromMaybe )

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden {front :: [(String, String)], back :: [(String, String)]} deriving Show

garden :: [String] -> String -> Garden
garden students plants = let [frontg, backg] = lines plants
                             go (facc, bacc) [] _ = Garden facc bacc
                             go (facc, bacc) (s:ss) (fg, bg) = go
                                ( (s, take 2 fg):facc
                                , (s, take 2 bg):bacc) ss (drop 2 fg, drop 2 bg)
                          
                          in go ([],[]) students (frontg, backg)
                        

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = 
    let studentPlantsStr = (++) <$>
                                lookup student (front garden) <*>
                                lookup student (back garden)
        studentPlants = maybe [] (fmap findPlant) studentPlantsStr
     in fromMaybe [] $ sequence studentPlants

findPlant :: Char -> Maybe Plant
findPlant 'C' = Just Clover
findPlant 'G' = Just Grass 
findPlant 'R' = Just Radishes
findPlant 'V' = Just Violets
findPlant _ = Nothing