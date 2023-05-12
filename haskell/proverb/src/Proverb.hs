module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = allFor x
recite tale@(x:_) = concat (zipWith forWant tale (tail tale)) ++ allFor x


forWant :: String -> String -> String
forWant x y = "For want of a " ++ x ++ " the " ++ y ++ " was lost.\n"

allFor :: String -> String
allFor x = "And all for the want of a " ++ x ++"."