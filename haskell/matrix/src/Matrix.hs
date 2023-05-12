module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import qualified Data.Vector as V
import Control.Monad
import Data.Vector (Vector)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)

newtype Matrix a = Matrix {getMatrix :: Vector (Vector a) } deriving (Eq, Show)

type MParser a = Parsec Void String a

data Expr a = Val a | Cons (Expr a) (Expr a) | End | ExprEof deriving Show

getNum :: MParser Int
getNum = read <$> (space *> many digitChar <* space)


exprInt :: MParser (Expr Int)
exprInt = do  Val <$> getNum

end :: MParser (Expr a)
end = do End <$ (space *> newline <*space)

eofPars :: MParser (Expr a)
eofPars = do ExprEof <$ (space *> eof <* space )



exprBuilder :: Expr a -> [[a]]
exprBuilder (Val a) = a
exprBuilder (Cons a b) = exprBuilder a : se
exprBuilder End = [] : se
exprBuilder ExprEof = [[]]


allParser :: MParser [[Int]]
allParser = exprBuilder [] <$> (exprInt <|> end <|> eofPars)

test = parse allParser  ""

cols :: Matrix a -> Int
cols m = maybe 0 V.length (getMatrix m V.!? 0)

column :: Int -> Matrix a -> Vector a
column x = (V.! (x-1)) <$> getMatrix

flatten :: Matrix a -> Vector a
flatten = join . getMatrix

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList [V.fromList x | x <- xss]

fromString :: Read a => String -> Matrix a
fromString "" = Matrix V.empty
fromString xs = fromList . (map read <$>) . map words . lines $ xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_, c) = Matrix . chunk c . flatten
                where chunk n v | V.null v = V.empty
                                | otherwise = V.take n v `V.cons` chunk n (V.drop n v)

row :: Int -> Matrix a -> Vector a
row x (Matrix v) = v V.! (x-1)

rows :: Matrix a -> Int
rows (Matrix v) = V.length v

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose m = Matrix $ let (_,c) = shape m
                           colVals = map (`column` m) [1 .. c]
                        in V.fromList colVals
