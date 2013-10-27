module SGMath.GCD where

import Data.Monoid
import Data.Ratio

gcd' :: Integral i => Ratio i -> Ratio i -> Ratio i
gcd' x y = (recip (commonDivisor%1)) * ( (gcd nomX' nomY') % 1)
	where
		nomX = numerator x ; denomX = denominator x
		nomY = numerator y ; denomY = denominator y
		nomX' = numerator ( x * ((commonDivisor % denomX)))
		nomY' = numerator ( y * ((commonDivisor % denomY)))
		commonDivisor = lcm denomX denomY

lcm' x y = x * y / gcd' x y

tupleFromRatio x = (numerator x, denominator x)

mod' :: Integral i => (Ratio i -> Ratio i -> Ratio i)
mod' = applyIntF mod

applyIntF f x y = let
	(numX,denX) = tupleFromRatio x
	(numY,denY) = tupleFromRatio y
	commonDivisor = lcm denX denY
	in
		(((ceiling $ x * (commonDivisor%1)) `f` (ceiling $ y * (commonDivisor%1)))%1) / (commonDivisor%1)

{-
newtype GCD = GCD { getGCD :: Rational}
	deriving(Show)
instance Monoid GCD where
	mempty = GCD 1
	(GCD x) `mappend` (GCD y) = GCD $ gcd' x y
-}
