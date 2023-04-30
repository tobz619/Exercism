module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.Maybe
import Data.List

type School = Map.Map Int [String]


add :: Int -> String -> School -> School
add gradeNum student school = case  Map.lookup gradeNum school of
                                 Nothing    -> Map.insert gradeNum [student] school
                                 (Just lis) -> Map.alter (const . pure . sort $ student: lis) gradeNum school

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = fromMaybe [] (Map.lookup gradeNum school)

sorted :: School -> [(Int, [String])]
sorted = Map.toList
