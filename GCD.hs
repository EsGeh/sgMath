module GCD where

import Data.Monoid
import Data.Ratio


gcd' :: Rational -> Rational-> Rational
gcd' x y = (recip (commonDivisor%1)) * ( (gcd nomX' nomY') % 1)
	where
		nomX = numerator x ; denomX = denominator x
		nomY = numerator y ; denomY = denominator y
		nomX' = numerator ( x * ((commonDivisor % denomX)))
		nomY' = numerator ( y * ((commonDivisor % denomY)))
		commonDivisor = lcm denomX denomY
lcm' x y = x * y / gcd' x y

newtype GCD = GCD { getGCD :: Rational}
	deriving(Show)
instance Monoid GCD where
	mempty = GCD 1
	(GCD x) `mappend` (GCD y) = GCD $ gcd' x y
