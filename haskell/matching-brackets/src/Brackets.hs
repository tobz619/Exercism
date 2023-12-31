{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = (== Just "") . foldr getPairs (Just "")
    where getPairs x acc 
            | isCloseBracket x = fmap (x:) acc
            | isOpenBracket x && maybe False null acc = Nothing
            | isOpenBracket x = if (head <$> acc) == pairBracket x
                                  then tail <$> acc
                                  else Nothing
            | otherwise = acc

pairBracket :: Char -> Maybe Char
pairBracket '{' = Just '}'
pairBracket '(' = Just ')'
pairBracket '[' = Just ']'
pairBracket _ = Nothing

isOpenBracket, isCloseBracket :: Char -> Bool
isOpenBracket = flip elem "{(["
isCloseBracket = flip elem "})]"