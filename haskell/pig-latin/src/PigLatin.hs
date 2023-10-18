module PigLatin (translate) where

translate :: String -> String
translate = unwords . map translateWord . words


translateWord :: [Char] -> [Char]
translateWord word@(x:xs) 
    | x `elem` "aeiou" = word ++ "ay"
    | take 2 word `elem` ["yt", "xr"] = word ++ "ay"
    | take 2 word == "xe" = xs ++ [x] ++ "ay"
    | x `elem` "y" = xs ++ [x] ++ "ay"
    | take 3 word `elem` ["thr", "sch"] = drop 3 word ++ take 3 word ++ "ay"
    | take 2 word `elem` ["ch", "qu", "th", "rh"] = drop 2 word ++ take 2 word ++ "ay"
    | take 2 xs == "qu" = drop 3 word ++ take 3 word ++ "ay"
    | otherwise = xs ++ [x] ++ "ay"