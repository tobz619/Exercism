module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as P (exp)
-- Data definition -------------------------------------------------------------
data Complex a = Complex {r :: a, i :: a} deriving (Show, Eq)

complex :: Num a => (a, a) -> Complex a
complex (a,b) = Complex a b

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate c = curry complex (r c) (negate (i c))

abs :: Floating a => Complex a -> a
abs c = sqrt (r c ^ 2 + i c ^ 2)

real :: Num a => Complex a -> a
real = r

imaginary :: Num a => Complex a -> a
imaginary = i

exp :: Floating a => Complex a -> Complex a
exp c =  let dist = P.exp (r c)
          in Complex (dist * cos (i c)) (dist * sin (i c))
-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = curry complex (a*c - b*d) (b*c+a*d)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = curry complex (a+c) (b+d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = curry complex (a-c) (b-d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) = curry complex 
                                         ( (a*c + b*d)/(c^2 + d^2) ) 
                                         ( (b*c - a*d)/(c^2 + d^2) )
