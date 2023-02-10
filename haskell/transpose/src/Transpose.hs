module Transpose (transpose) where
import Data.Char
import qualified Data.List as L

transpose :: [String] -> [String]
transpose [] = [] 
transpose xs 
      | all null xs = []
      | otherwise = L.unfoldr anoF xs : (transpose . map (drop 1)) xs



anoF [] = Nothing
anoF ([]:xs) = Just (' ', xs)
anoF (x@(y:_):xs) = Just (y, xs)
                  

printTranspose =  mapM_ putStrLn . transpose

ex1 = [ "The fourth line." , "The fifth line."]

px1 = printTranspose ex1