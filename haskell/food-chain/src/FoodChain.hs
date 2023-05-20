module FoodChain (song) where

song :: String
song = unlines songBuilder


oldLady :: [Char] -> [Char]
oldLady x = "I know an old lady who swallowed a " ++ x ++ "."

swallowed :: [Char] -> [Char] -> String
swallowed _ [] = ""
swallowed x "spider" = "She swallowed the " ++ x ++ " to catch the spider that " ++
                        drop 3 (rhyme "spider")
swallowed x y = "She swallowed the " ++ x ++ " to catch the " ++ y ++ "."


swallowChain :: [String] -> [String]
swallowChain ["fly"] = [rhyme "fly"]
swallowChain (x:y:xs) = swallowed x y : swallowChain (y:xs) 

rhyme :: String -> String
rhyme "fly" = "I don't know why she swallowed the fly. Perhaps she'll die.\n"
rhyme "spider" = "It wriggled and jiggled and tickled inside her."
rhyme "bird" = "How absurd to swallow a bird!"
rhyme "cat" = "Imagine that, to swallow a cat!"
rhyme "dog" = "What a hog, to swallow a dog!"
rhyme "goat" = "Just opened her throat and swallowed a goat!"
rhyme "cow" = "I don't know how she swallowed a cow!"
rhyme "horse" = "She's dead, of course!"
rhyme _ = ""

order :: [String]
order = ["fly","spider","bird","cat","dog","goat","cow","horse"]

select :: String -> [String]
select "horse" = ["horse"]
select x = dropWhile (/= x) (reverse order)

verse :: [String] -> [[Char]]
verse ["horse"] = [oldLady "horse", rhyme "horse"]
verse ["fly"] = [oldLady "fly", rhyme "fly"]
verse order@(x:y:xs) = [oldLady x, rhyme x] ++ swallowChain order

songBuilder :: [[Char]]
songBuilder = concatMap (verse . select) order 