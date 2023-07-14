module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = op (toBin n)
    where op [1] = ["wink"]
          op [1,x] = "double blink" : op [x]
          op [1,x,y] = "close your eyes" : op [x,y]
          op [1,x,y,z] = "jump" : op [x,y,z]
          op [1,a,b,c,d] = op [a,b,c,d]
          op (0:xs) = op xs
          op _ = []


toBin :: Integral a => a -> [a]
toBin 0 = [0]
toBin n = go n []
    where go 0 r = r
          go k rs = go (k `div` 2) (k `mod` 2:rs)