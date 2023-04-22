module Poker (bestHands) where

import Text.Megaparsec
import Text.Parsec.Combinator
import Data.Void (Void)

type CardParser = Parsec Void String

bestHands :: [String] -> Maybe [String]
bestHands = error "You need to implement this function!"
