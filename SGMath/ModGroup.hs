{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, Rank2Types, ScopedTypeVariables, FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, Rank2Types, FlexibleContexts, UndecidableInstances #-}
module SGMath.ModGroup (
	ModGroup(),
	ModGroupRatio(),
	mg, mgFix, mgWith,
	mg', mgFix', mgWith',
) where

import SGMath.GCD
import SGCard
import Data.Ratio


class Wrapper wrapper content | wrapper -> content where
	value :: wrapper -> content

data ModGroupRatio max = ModGroupRatio {
	mgRatioGetValue :: Ratio Int
}
data ModGroup max = ModGroup {
	mgGetValue :: Int
}
instance Wrapper (ModGroup max) (Int) where
	value = mgGetValue
instance Wrapper (ModGroupRatio max) (Ratio Int) where
	value = mgRatioGetValue

instance (Card max) => Num (ModGroup max) where
	l + r = ModGroup $ modul max (value l + value r)
		where max = toInt (undefined :: max)
	l * r = ModGroup $ modul max (value l * value r)
		where max = toInt (undefined :: max)
	abs a = ModGroup $ abs $ value a
	signum a = ModGroup $ signum $ value a
	fromInteger i = ret
		where
			ret = ModGroup $ modul max (fromIntegral i)
			max = toInt (undefined :: max)
instance (Card max) => Eq (ModGroup max) where
	a == b =
		(value a) == (value b)
instance (Card max) => Show (ModGroup max) where
	show x = show $ value x

instance (FractionClass max) => Num (ModGroupRatio max) where
	l + r = ModGroupRatio $ modul' max (value l + value r)
		where max = toFrac (undefined :: max)
	l * r = ModGroupRatio $ modul' max (value l * value r)
		where max = toFrac (undefined :: max)
	abs a = ModGroupRatio $ abs $ value a
	signum a = ModGroupRatio $ signum $ value a
	fromInteger i = ret
		where
			ret = ModGroupRatio $ modul' max (fromIntegral i)
			max = toFrac (undefined :: max)
instance (FractionClass max) => Eq (ModGroupRatio max) where
	a == b =
		(value a) == (value b)
instance (FractionClass max) => Show (ModGroupRatio max) where
	show x = show $ value x

-- packing a value with a compile time fixed modulus
mgFix :: forall max . Card max => max -> Int -> ModGroup max
mgFix _ val = mg val

-- |
mgWith :: Int -> (forall max . (Card max) => ModGroup max) -> Int
mgWith mod f = withCard mod (\modCard -> value $ withMod modCard f)
	where
		withMod :: max -> ModGroup max -> ModGroup max
		withMod mod val = val

test :: Card m => m -> [Int]
test m = map (\val -> mgWith (toInt m) $ mg val ) $ [1,3..]

-- packing to a polymorphic modulus
mg :: forall max . Card max => Int -> ModGroup max
mg val = ModGroup $ modul max val
	where
		max = toInt (undefined :: max)

mgFix' :: forall maxVal . FractionClass maxVal => maxVal -> Ratio Int -> ModGroupRatio maxVal
mgFix' _ val = mg' val 

mgWith' :: Ratio Int -> (forall ctRatio . FractionClass ctRatio => ModGroupRatio ctRatio) -> Ratio Int
mgWith' mod val = withFrac mod (\modRatio -> value $ withMod modRatio val)
	where
		withMod :: max -> ModGroupRatio max -> ModGroupRatio max
		withMod mod val = val
--mgWith' maxVal val = withFrac maxVal (\maxVal -> value $ mg' maxVal val)

mg' :: forall maxVal . FractionClass maxVal => Ratio Int -> ModGroupRatio maxVal
mg' val = ModGroupRatio $ modul' max val
	where
		max = toFrac (undefined :: maxVal)

--mgWith_' :: Ratio Int -> Ratio Int -> ModGroupRatio maxVal

{-
withMod maxVal val = 
-}

{-
toFrac' :: forall num den .(Card num, Card den) => CTFraction num den -> Ratio Int
toFrac' _ = num % den
	where
		num = toInt (undefined :: num)
		den = toInt (undefined :: den)
-}


{-
import qualified SGCard.Card as Card
import SGCard.Card hiding(toInt)

data ModGroupRatio m = ModGroupRatio { maxVal :: m, toInt :: Int }
instance (Card.Card m) => Num (ModGroupRatio m) where
	l + r = ModGroupRatio m $ modul (Card.toInt m) (toInt l + toInt r)
		where m = maxVal l
	l * r = ModGroupRatio m $ modul (Card.toInt m) $ toInt l + toInt r
		where m = maxVal l-- Card.toInt $ (undefined :: m)
	abs a = mapModGroupRatio abs a
	signum a = mapModGroupRatio signum a
	fromInteger = ModGroupRatio undefined . fromIntegral
instance (Card.Card m) => Eq (ModGroupRatio m) where
	a == b =
		(toInt a) == (toInt b)
instance (Card.Card m) => Show (ModGroupRatio m) where
	show val = show $ toInt val

mg :: (Card m) => m -> Int -> ModGroupRatio m
mg m = (+ (ModGroupRatio m 0)) . ModGroupRatio m

mapModGroupRatio f a = ModGroupRatio (maxVal a) $ f $ toInt a

-}
modul m val = (flip mod) m val
modul' m val = (flip mod') m val
