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

type Vec a = Array Int a

dot :: Num a => Vec a -> Vec a -> a
dot lhs rhs = sum $ zipWith (*) (elems lhs) (elems rhs)

{-
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

instance Arbitrary a => Arbitrary (Vec a) where
    arbitrary = oneof [fmap Vec arbitrary, pure Zeros]

index :: Vec a -> Int -> a
index Zeros _ = 0
index (Vec arr) i = arr ! i
-}

instance VectorSpace a => Num (Dual a) where
    Dual x dx + Dual y dy = Dual (x + y) (dx `add` dy)
    Dual x dx * Dual y dy = Dual (x * y) ((x `scale` dy) `add` (y `scale` dx))
    fromInteger i = Dual (fromInteger i) zero
    negate (Dual x dx) = Dual (-x) ((-1) `scale` dx)
    abs _ = undefined
    signum = undefined

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
f3 x = (x `dot` x) + 10

df3 :: Vec Double -> Vec Double
df3 x = let n = numElements x
            dxs = flip fmap [0..n-1] $ \i ->
                listArray (0, n-1) $ [Dual (x!j) (if i == j then 1 else 0) | j <- [0..n-1]]
         in listArray (0, n-1) $ fmap (derivative . f3) dxs

-- f = x1^2 + x2^2 + x3^2 + ... + xn^2 + 10
-- df = [2x1, 2x2, 2x3, ..., 2xn]
df3' :: Vec Double -> Vec Double
df3' x = listArray (0, n-1) [2 * el | el <- elems x]
    where n = numElements x

f3_prop :: [Double] -> Bool
f3_prop listX = let x = listArray (0, length listX - 1) listX
                 in df3 x == df3' x

f3_check = quickCheck f3_prop

data Delta = Zero | Add Delta Delta | Scale Double Delta | OneHot Int

instance VectorSpace Delta where
    zero = Zero
    add = Add
    scale = Scale

oneHot :: (Num a, Ix i) => (i, i) -> i -> Array i a
oneHot bounds i = listArray bounds $ [if i == j then 1 else 0 | j <- range bounds]

eval :: (Num a, VectorSpace a) => (Int, Int) -> Delta -> Vec a
eval bounds (OneHot i) = oneHot bounds i
eval bounds (Add lhs rhs) = listArray bounds $ zipWith add (elems $ eval bounds lhs) (elems $ eval bounds rhs)
eval bounds (Scale factor delta) = fmap (factor `scale`) $ eval bounds delta
eval bounds Zero = listArray bounds [0 | _ <- range bounds]

df3_2 :: Vec Double -> Vec Double
df3_2 x = eval (0, n-1) $ derivative $ f3 dxs
    where dxs = listArray (0, n-1) $ [Dual (x!i) (OneHot i) | i <- [0..n-1]]
          n = numElements x

f3_2_prop :: [Double] -> Bool
f3_2_prop listX = df3_2 x == df3' x
    where x = listArray (0, n-1) listX
          n = length listX

f3_2_check = quickCheck f3_2_prop

someFunc :: IO ()
someFunc = putStrLn "someFunc"

