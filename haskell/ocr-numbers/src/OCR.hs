module OCR (convert) where

import Data.List
import Data.Char (intToDigit)

convert :: String -> String
convert xs = intercalate "," $ map unocr allLines
            where allLines = splitEvery 4 (lines xs)
                  splitEvery _ []  = []
                  splitEvery n xs  = take n xs : splitEvery n (drop n xs)

makeOCR :: Char -> [String]
makeOCR '0' = [" _ ",
               "| |",
               "|_|",
               "   "]

makeOCR '1' = ["   ",
               "  |",
               "  |",
               "   "]

makeOCR '2' = [" _ "
              ," _|"
              ,"|_ "
              ,"   "]

makeOCR '3' = lines  " _ \n _|\n _|\n   "

makeOCR '4' = lines "   \n|_|\n  |\n   "

makeOCR '5' = lines " _ \n|_ \n _|\n   "

makeOCR '6' = lines " _ \n\
                    \|_ \n\
                    \|_|\n\
                    \   "

makeOCR '7' = lines " _ \n\
                    \  |\n\
                    \  |\n\
                    \   "

makeOCR '8' = lines " _ \n\
                    \|_|\n\
                    \|_|\n\
                    \   "

makeOCR '9' = lines " _ \n\
                    \|_|\n\
                    \ _|\n\
                    \   "

makeOCR _ = []



prep :: String -> [String]
prep = split ','
      where split _ [] = []
            split c xs = let
             in case break (== c) xs of
                  (chunk,[]) -> [chunk]
                  (chunk,rest) -> chunk : split c (tail rest)

ocrLine :: String -> [[String]]
ocrLine = map makeOCR

ocrRow :: [[String]] -> [[String]]
ocrRow = foldr (zipWith (:)) [[],[],[],[]]



ocr :: String -> [Char]
ocr =  concatMap (unlines . map concat . ocrRow . ocrLine) . prep

unocr :: [String] -> [Char]
unocr = map ocrToText . unocrLine

unocrLine :: [String] -> [[String]]
unocrLine xs | all null xs = []
             | otherwise = map (take 3) xs : (unocrLine . map (drop 3)) xs


ocrToText :: [String] -> Char
ocrToText xs = maybe '?'  intToDigit (elemIndex xs (map makeOCR "01234567890"))


printOcrs :: String -> IO ()
printOcrs = putStr . ocr