module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins xs = transcode (Just []) (map codToPro (toCodon xs))
            where transcode acc ("STOP":_) = acc
                  transcode _   ("Err":_) = Nothing
                  transcode acc (x:xs) = transcode (fmap (x :) acc) xs
                  transcode acc [] = acc

toCodon :: [a] -> [[a]]
toCodon = splitEvery 3
    where  splitEvery _ [] = []
           splitEvery n xs = take n xs : splitEvery n (drop n xs)

codToPro :: String -> String
codToPro x
 | elem x ["AUG"] = "Methionine"
 | elem x ["UUU", "UUC"] = "Phenylalanine"
 | elem x ["UUA", "UUG"] = "Leucine"
 | elem x ["UCU", "UCC", "UCA", "UCG"] = "Serine"
 | elem x ["UAU", "UAC"] = "Tyrosine"
 | elem x ["UGU", "UGC"] = "Cysteine"
 | elem x ["UGG"] = "Tryptophan"
 | elem x ["UAA", "UAG", "UGA"] = "STOP"
 | otherwise = "Err"