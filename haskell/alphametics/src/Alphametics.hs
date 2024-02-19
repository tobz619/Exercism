{-# LANGUAGE BangPatterns #-}
module Alphametics (solve) where

import qualified Data.Map as Map
import Data.List ( delete, nub, transpose, foldl', find)
import Data.Char ( intToDigit, isAlpha )
import Data.Maybe (fromMaybe, mapMaybe, maybeToList, listToMaybe)
import Text.Read (readMaybe)
import Text.Megaparsec
    ( parse, many, (<|>), Parsec, MonadParsec(try) )
import Text.Megaparsec.Char ( char, space, upperChar, string )
import Data.Void ( Void )
import Data.Monoid (Sum(Sum), Product (Product)) -- for the future
import Debug.Trace (traceShowId)


mkSumMap :: Map.Map Char Integer -> Map.Map Char (Sum Integer)
mkSumMap = Map.insert ' ' mempty . Map.map Sum -- for the future
mkProdMap :: Map.Map Char Integer -> Map.Map Char (Product Integer)
mkProdMap = Map.insert ' ' mempty . Map.map Product -- for the future

type PossibleChars = Map.Map Char [Int]
type Parser = Parsec Void String

data Operator = Plus | Mult | Exp deriving Show
data Expr = Value String
          | Expr Operator Expr Expr
          deriving Show

data Equation = Equation { eExpr :: Expr
                         , result :: String } deriving Show

solve :: String -> Maybe [(Char, Int)]
solve puzzle = let res = mkValuePair puzzle
                   col = mkColumns puzzle
                   maps = maybe [] (\(cols, inpRes) -> loop addChars cols inpRes (initChars puzzle)) col

                in res >>= \(inpCol, resStr) -> getValidInp inpCol resStr maps

setCharVariants :: Char -> PossibleChars -> Maybe [PossibleChars]
setCharVariants c charMap = do lis <- Map.lookup c charMap
                               return $! map (updateMap charMap c) lis


setAllCharVariants :: String -> PossibleChars -> Maybe [PossibleChars]
setAllCharVariants inpString charMap = go inpString [charMap]
    where go "" charMapList   = pure charMapList
          go (s:str) charMapList  = traverse (setCharVariants s) charMapList >>= go str . concat


updateMap :: PossibleChars -> Char -> Int -> PossibleChars
updateMap mp c i = setChar c i mp
        where setChar ch v = Map.mapWithKey (selector ch)
                where selector ' ' _ xs = xs
                      selector _ ' ' xs = xs
                      selector toChange key xs
                        | key == toChange = [v]
                        | otherwise = delete v xs

validAdd :: String -> Char -> Int -> PossibleChars -> Maybe [(Int, PossibleChars)]
validAdd cs resChar carry charMap = do
    as <- traverse (`Map.lookup` charMap) cs
    resVals <- resChar `Map.lookup` charMap

    return [ ( c, toReturn ) |
        v1 <- sequence as,
        let (c, resDig) = (`divMod` 10) . (carry +) $ sum v1,
        resDig `elem` resVals,
        let toReturn = updateMap charMap resChar resDig
        ]


addChars :: String -> Char -> Int -> PossibleChars -> Maybe [(Int, PossibleChars)]
addChars cs resChar carry charMap =
    let eligMaps = setAllCharVariants (resChar: cs) charMap
    in concat . mapMaybe (validAdd cs resChar carry) <$> eligMaps

processFun :: (String  -> Char -> Int -> PossibleChars -> Maybe [(Int, PossibleChars)])
            -> String -> Char -> Int -> PossibleChars -> [(Int, PossibleChars)]
processFun f str ch car charMap = mapMaybe checkNull results
        where results = fromMaybe [] $ f str ch car charMap
              checkNull (c,mp) = (,) c <$> traverse (\x -> if null x then Nothing else Just x) mp

loop :: (String -> Char -> Int -> PossibleChars -> Maybe [(Int, PossibleChars)]) -> [String] -> [Char] -> PossibleChars -> [Map.Map Char [Int]]
loop = go 0
    where go _ _ _ [] mp = pure mp
          go _ _ [] _ mp = pure mp
          go carry f (s:strs) (c:chrs) !charMap = let
            !carryRes = processFun f s c carry charMap
            in concatMap (\ ~(newCar, newMap) -> go newCar f strs chrs newMap) carryRes

standardiseInps :: [String] -> String -> ([String], String)
standardiseInps inps res = (transpose lined, normaledRes)
    where (normaledRes: lined) = map (reverse . addPadding padAmount) (res:inps)
          padAmount = maximum . map length $ (res:inps)
          addPadding amount x = replicate (amount - length x) ' ' ++ x


-- >>> pairColumns ["SEND","MORE"] "MONEY"
-- (["DE","NR","EO","SM","  "],"YENOM")
pairColumns :: [String] -> String -> ([String], [Char])
pairColumns inps = unzip . uncurry zip . standardiseInps inps


initChars :: String -> Map.Map Char [Int]
initChars xs = Map.insert ' ' [0] $ foldr initiator Map.empty ((nub.filter isAlpha) xs)
        where initiator x = Map.insert x [0..9]


validateAns :: [String] -> String -> PossibleChars -> Bool
validateAns inpStrings resString charMap =
    let numString chars = concat <$> traverse (`Map.lookup` charMap) chars
        readNum = readMaybe . map intToDigit :: [Int] -> Maybe Int
        inpStrNums = traverse numString inpStrings
        resStrNum = numString resString
        resNum = readNum =<< resStrNum
        inpNums = traverse readNum =<< inpStrNums


     in fmap sum inpNums == resNum && noZeroLead ((:) <$> resStrNum <*> inpStrNums)
            where noZeroLead strs = Just 0 `notElem` (map listToMaybe . concat $ strs)

-- >>> solve "A + A + A + A + A + A + A + A + A + A + A + A == BCD"
-- Just [('A',9),('B',1),('C',0),('D',8)]
getValidInp :: [String] -> String -> [PossibleChars] -> Maybe [(Char, Int)]
getValidInp strs res cands = Map.toList . Map.map head . Map.delete ' ' <$> find (validateAns strs res) cands

addParse :: Parser Expr
addParse = Expr Plus <$> valueParse <* (space *> char '+' <* space) <*> pExpr

multParse :: Parser Expr
multParse = Expr Plus <$> valueParse <* (space *> char '*' <* space) <*> pExpr

pExpr :: Parser Expr
pExpr = space *> (try addParse <|> try multParse <|> valueParse) <* space

valueParse :: Parser Expr
valueParse = Value <$> (space *> many upperChar)

resParse :: Parser String
resParse = do _ <- space *> string "==" <* space
              many upperChar


equationParse :: Parser Equation
equationParse = Equation <$> pExpr <*> resParse


exprToStringList :: Expr -> [String]
exprToStringList = go []
        where go acc (Value s) = s:acc
              go acc (Expr _ a b) = go acc a <> go acc b

mkValuePair :: String -> Maybe ([String], String)
mkValuePair s = case parse equationParse "" s of
                    Left _ -> Nothing
                    Right t -> Just (exprToStringList (eExpr t), result t)

mkColumns :: String -> Maybe ([String], [Char])
mkColumns = fmap (uncurry pairColumns). mkValuePair


stupidThing = "THIS + A + FIRE + THEREFORE + FOR + ALL + HISTORIES + I + TELL + A + TALE + THAT + FALSIFIES + ITS + TITLE + TIS + A + LIE + THE + TALE + OF + THE + LAST + FIRE + HORSES + LATE + AFTER + THE + FIRST + FATHERS + FORESEE + THE + HORRORS + THE + LAST + FREE + TROLL + TERRIFIES + THE + HORSES + OF + FIRE + THE + TROLL + RESTS + AT + THE + HOLE + OF + LOSSES + IT + IS + THERE + THAT + SHE + STORES + ROLES + OF + LEATHERS + AFTER + SHE + SATISFIES + HER + HATE + OFF + THOSE + FEARS + A + TASTE + RISES + AS + SHE + HEARS + THE + LEAST + FAR + HORSE + THOSE + FAST + HORSES + THAT + FIRST + HEAR + THE + TROLL + FLEE + OFF + TO + THE + FOREST + THE + HORSES + THAT + ALERTS + RAISE + THE + STARES + OF + THE + OTHERS + AS + THE + TROLL + ASSAILS + AT + THE + TOTAL + SHIFT + HER + TEETH + TEAR + HOOF + OFF + TORSO + AS + THE + LAST + HORSE + FORFEITS + ITS + LIFE + THE + FIRST + FATHERS + HEAR + OF + THE + HORRORS + THEIR + FEARS + THAT + THE + FIRES + FOR + THEIR + FEASTS + ARREST + AS + THE + FIRST + FATHERS + RESETTLE + THE + LAST + OF + THE + FIRE + HORSES + THE + LAST + TROLL + HARASSES + THE + FOREST + HEART + FREE + AT + LAST + OF + THE + LAST + TROLL + ALL + OFFER + THEIR + FIRE + HEAT + TO + THE + ASSISTERS + FAR + OFF + THE + TROLL + FASTS + ITS + LIFE + SHORTER + AS + STARS + RISE + THE + HORSES + REST + SAFE + AFTER + ALL + SHARE + HOT + FISH + AS + THEIR + AFFILIATES + TAILOR + A + ROOFS + FOR + THEIR + SAFE == FORTRESSES"

ex1 =  solve stupidThing