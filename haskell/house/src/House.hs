module House (rhyme) where
import Data.List (inits)

rhyme :: String
rhyme = init . unlines . map verseMaker . filter (not . null) . inits $ order


pairs :: String -> String
pairs "house that Jack built" = ""
pairs "malt" = "lay in"
pairs "rat" = "ate"
pairs "cat" = "killed"
pairs "dog" = "worried"
pairs "cow with the crumpled horn" = "tossed"
pairs "maiden all forlorn" = "milked"
pairs "man all tattered and torn" = "kissed"
pairs "priest all shaven and shorn" = "married"
pairs "rooster that crowed in the morn" = "woke"
pairs "farmer sowing his corn" = "kept"
pairs "horse and the hound and the horn" = "belonged to"
pairs _ = ""

order = [ "house that Jack built", "malt", "rat", "cat", "dog", "cow with the crumpled horn"
        , "maiden all forlorn", "man all tattered and torn","priest all shaven and shorn"
        , "rooster that crowed in the morn", "farmer sowing his corn"
        , "horse and the hound and the horn"
        ]

verseMaker :: [String] -> String
verseMaker [] = ""
verseMaker inps = "This is" ++ unlines (lineMaker inps)

lineMaker :: [String] -> [String]
lineMaker = foldr builder [] . reverse
        where   builder x acc
                 | x == "house that Jack built" = " the house that Jack built." : acc
                 | otherwise  = let currentLine = unlines acc
                                 in (" the " ++ x) : lines ("that " ++ pairs x ++ currentLine)