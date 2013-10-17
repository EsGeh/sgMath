module SGMath.ModGroup(
	ModGroup(),
	mg,
	modul
) where

import qualified SGCard.Card as Card
import SGCard.Card hiding(toInt)

data ModGroup m = ModG { maxVal :: m, toInt :: Int }
instance (Card.Card m) => Num (ModGroup m) where
	l + r = ModG m $ modul (Card.toInt m) (toInt l + toInt r)
		where m = maxVal l
	l * r = ModG m $ modul (Card.toInt m) $ toInt l + toInt r
		where m = maxVal l-- Card.toInt $ (undefined :: m)
	abs a = mapModG abs a
	signum a = mapModG signum a
	fromInteger = ModG undefined . fromIntegral
{-instance (Card.Card m) => Enum (ModGroup m) where
	toEnum i = mg (maxVal val) val
	fromEnum = toInt
	--succ n = mapModG (+1) n -}
instance (Card.Card m) => Eq (ModGroup m) where
	a == b =
		(toInt a) == (toInt b)
instance (Card.Card m) => Show (ModGroup m) where
	show val = show $ toInt val

mg :: (Card m) => m -> Int -> ModGroup m
mg m = (+ (ModG m 0)) . ModG m

mapModG f a = ModG (maxVal a) $ f $ toInt a
modul mod' val = (flip mod) mod' val
