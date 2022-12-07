module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Data.List

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Eq, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor
 | ohms resistor < 10^3 = show (ohms resistor) ++ " ohms"
 | ohms resistor < 10^6 = show (ohms resistor `div` 10^3) ++ " kiloohms"
 | ohms resistor < 10^9 = show (ohms resistor `div` (10^6)) ++ " megaohms"
 | otherwise = show (ohms resistor `div` (10^9)) ++ " gigaohms"

ohms :: Resistor -> Int
ohms resistor = let (f,s,t) = bands resistor
                    res     = do fi <- (*10) <$> colorVal f
                                 se <- colorVal s
                                 th <- (10^) <$> colorVal t
                                 return $ (fi + se) * th
                    in case res of Just x -> x
                                   _ -> 0


colorVal :: Color -> Maybe Int
colorVal c = elemIndex c [Black .. White]