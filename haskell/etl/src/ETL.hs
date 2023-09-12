module ETL (transform) where

import Data.Char
import Data.Map (Map, fromList, toList)
import Control.Monad ((<=<))

transform :: Map a String -> Map Char a
transform = fromList . (transfomer <=< toList)
            where transfomer (k, strs) = [(toLower c, k) | c <- strs]


