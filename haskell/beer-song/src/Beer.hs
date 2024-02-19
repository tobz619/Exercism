module Beer (song) where
import Data.Char (toLower)

song :: String
song = init . unlines . map verse $ [99, 98 .. 0]

verse :: Int -> String
verse x = unlines [beer x, pass x]

beer :: Int -> String
beer x = check x ++ " of beer on the wall, " ++ map toLower (check x) ++ " of beer."

pass :: Int -> String
pass 0 = "Go to the store and buy some more, 99 bottles of beer on the wall."
pass x = "Take " ++ one x ++ " down and pass it around, " ++ map toLower (check (x-1)) ++ " of beer on the wall."

check :: Int -> String
check 0 = "No more bottles"
check 1 = "1 bottle"
check x = show x ++ " bottles"

one :: Int -> String
one 1 = "it"
one _ = "one"