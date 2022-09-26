{-# LANGUAGE FlexibleInstances #-}

module Lib where

import GHC.Arr hiding (index)
import Data.Function
import Test.QuickCheck (quickCheck, Arbitrary(..), oneof)

data Dual a = Dual Double a

derivative :: Dual a -> a
derivative (Dual _ f') = f'

class VectorSpace a where
    add :: a -> a -> a
    scale :: Double -> a -> a
    zero :: a

instance VectorSpace Double where
    add = (+)
    scale = (*)
    zero = 0

instance VectorSpace a => VectorSpace (a, a, a) where
    add (x, y, z) (x', y', z') = (x `add` x', y `add` y', z `add` z')
    scale factor (x, y, z) = (factor `scale` x, factor `scale` y, factor `scale` z)
    zero = (zero, zero, zero)

instance VectorSpace a => VectorSpace (a, a) where
    add (x, y) (x', y') = (x `add` x', y `add` y')
    scale factor (x, y) = (factor `scale` x, factor `scale` y)
    zero = (zero, zero)

data Vec a = Vec (Array Int a)
           | Zeros
           deriving Eq

instance VectorSpace a => VectorSpace (Vec a) where
    add lhs Zeros = lhs
    add Zeros rhs = rhs
    add (Vec lhs) (Vec rhs)
      | bounds lhs == bounds rhs = Vec $ accumArray add 0 (bounds lhs) elems
      | otherwise = error "Bounds don't match"
      where elems = assocs lhs <> assocs rhs

    scale _ Zeros = Zeros
    scale factor (Vec vec) = Vec $ fmap (factor `scale`) vec
    zero = Zeros

instance VectorSpace a => Num (Dual a) where
    Dual x dx + Dual y dy = Dual (x + y) (dx `add` dy)
    Dual x dx * Dual y dy = Dual (x * y) ((x `scale` dy) `add` (y `scale` dx))
    fromInteger i = Dual (fromInteger i) zero
    negate (Dual x dx) = Dual (-x) ((-1) `scale` dx)
    abs _ = undefined
    signum = undefined

instance Arbitrary a => Arbitrary (Vec a) where
    arbitrary = oneof [fmap Vec arbitrary, pure Zeros]

index :: Vec a -> Int -> a
index Zeros _ = 0
index (Vec arr) i = arr ! i

f1 :: Num a => a -> a
f1 x = x * x + 10

df1 :: Double -> Double
df1 x = derivative $ f1 (Dual x 1)

df1' :: Double -> Double
df1' x = 2 * x

f1_prop :: Double -> Bool
f1_prop x = df1 x == df1' x

f1_check = quickCheck f1_prop

f2 :: Num a => (a, a) -> a
f2 (x, y) = x * y + 10

df2 :: (Double, Double) -> (Double, Double)
df2 (x, y) = let dfdx = derivative $ f2 (Dual x 1, Dual y 0)
                 dfdy = derivative $ f2 (Dual x 0, Dual y 1)
              in (dfdx, dfdy)

df2_2 :: (Double, Double) -> (Double, Double)
df2_2 (x, y) = derivative $ f2 (Dual x (1, 0), Dual y (0, 1))

df2' :: (Double, Double) -> (Double, Double)
df2' (x, y) = (y, x)

f2_prop :: (Double, Double) -> Bool
f2_prop tup = let handcrafted = df2' tup
               in df2 tup == handcrafted && df2_2 tup == handcrafted

f2_check = quickCheck f2_prop

f3 :: Num a => Vec a -> a
f3 x = x `dot` x + 10
  where dot :: Num a => Vec a -> Vec a -> a
        dot Zeros _ = 0
        dot _ Zeros = 0
        dot (Vec lhs) (Vec rhs) = sum $ zipWith (*) (elems lhs) (elems rhs)

df3 :: Vec Double -> Vec Double
df3 x = let n = numElements x
            dxs = listArray (0, n-1) [Dual (x `index` i) (oneHot (0, n-1) i) | i <- [0..n-1]]
         in f3 dxs

-- x1^2 + x2^2 + x3^3 + 10
-- df/dx1 = 2x1
df3' :: Vec Double -> Vec Double
df3' x = listArray (0, n-1) [2 * el | el <- elems x]
  where n = numElements x

f3_prop :: Vec Double -> Bool
f3_prop x = df3 x == df3' x

f3_check = quickCheck f3_prop

oneHot :: Ix i => (i, i) -> i -> Array i a
oneHot bounds i = array bounds [(j, if j == i then 1 else 0) | j <- range bounds]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
