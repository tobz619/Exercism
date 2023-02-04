module OCR (convert) where
    
import Data.List
import Data.Char (intToDigit)

convert :: String -> String
convert xs = error "You need to implement this function."

makeOCR :: Char -> [String]
makeOCR '0' = [" _ ",
               "| |",
               "|_|",
               "   "]

makeOCR '1' = ["   ","  |","  |","   "]

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


makeOCR ',' = lines "\n\n\n"

ocrMaker = map makeOCR



ocrRow xs = foldr1 (zipWith (++)) xs


ocrToText :: [String] -> Char
ocrToText xs = maybe '?'  intToDigit (elemIndex xs (map makeOCR "01234567890,"))


-- printOcrs = mapM_ putStrLn . ocr