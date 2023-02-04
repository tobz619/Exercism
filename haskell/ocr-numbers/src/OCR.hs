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


ocrMaker :: [Char] -> [[[String]]]
ocrMaker xs = go [] xs
             where go acc (',':xs) = reverse acc : go [] xs
                   go acc (x:xs)   = go (makeOCR x: acc) xs
                   go acc []       = [reverse acc]


ocrRow :: [[[String]]] -> [[String]]
ocrRow [] = []
ocrRow (x:xs) = foldr1 (zipWith (++)) x : ocrRow xs


ocr :: String -> String
ocr =  concat . map unlines . ocrRow . ocrMaker

unocr = map ocrToText . unocrLine

unocrLine xs | all null xs = []
             | otherwise = map (take 3) xs : (unocrLine . map (drop 3)) xs
        

ocrToText :: [String] -> Char
ocrToText xs = maybe '?'  intToDigit (elemIndex xs (map makeOCR "01234567890"))


printOcrs = putStr . ocr