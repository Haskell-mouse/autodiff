module Sized where

import GHC.TypeLits
import Data.Kind
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.GADT.Show.TH (deriveGShow)
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Compare (GCompare, GEq(..))
import qualified Numeric.LinearAlgebra.Static as LinAlg
import qualified Numeric.LinearAlgebra as LinAlg (Additive(..))
import Numeric.LinearAlgebra.Static (L)

import Data.Proxy
import Type.Reflection
import Control.Monad (guard)

import Data.Maybe (fromMaybe, fromJust, isJust)

import Debug.Trace
import Test.QuickCheck

type Mat :: (Nat, Nat) -> Type
data Mat tup where
    Mat :: (KnownNat n, KnownNat m) => L n m -> Mat '(n, m)

deriving instance (KnownNat n, KnownNat m) => Show (Mat '(n, m))

instance GEq Mat where
    geq (Mat lhs) (Mat rhs) = do
        (Refl, Refl) <- checkEqSizes lhs rhs
        guard (LinAlg.extract lhs == LinAlg.extract rhs)
        return Refl
        where checkEqSizes :: forall n m r p. (KnownNat n, KnownNat m, KnownNat r, KnownNat p) => L n m -> L r p -> Maybe (n :~: r, m :~: p)
              checkEqSizes _ _ = do
                  Refl <- typeOf (Proxy :: Proxy n) `geq` typeOf (Proxy :: Proxy r)
                  Refl <- typeOf (Proxy :: Proxy m) `geq` typeOf (Proxy :: Proxy p)
                  return (Refl, Refl)

idMat :: KnownNat n => Mat '(n, n)
idMat = Mat LinAlg.eye

zeroMat :: (KnownNat n, KnownNat m) => Mat '(n, m)
zeroMat = Mat $ LinAlg.build (\_ _ -> 0)

type SizedSemiring :: ((Nat, Nat) -> Type) -> Constraint
class SizedSemiring d where
    plus  :: (KnownNat n, KnownNat m) => d '(n, m) -> d '(n, m) -> d '(n, m)
    times :: (KnownNat n, KnownNat m, KnownNat k) => d '(n, k) -> d '(k, m) -> d '(n, m)
    tr    :: (KnownNat n, KnownNat m) => d '(n, m) -> d '(m, n)
    fromMat :: (KnownNat n, KnownNat m) => Mat '(n, m) -> d '(n, m)

instance SizedSemiring Mat where
    (Mat lhs) `plus` (Mat rhs) = Mat $ lhs `LinAlg.add` rhs
    (Mat lhs) `times` (Mat rhs) = Mat $ (LinAlg.<>) lhs rhs
    tr (Mat mat) = Mat $ LinAlg.tr mat
    fromMat = id

infixl 7 `times`
infixl 6 `plus`

{-
tr :: Mat n m -> Mat m n
tr _ = Mat

matmul :: Mat n k -> Mat k m -> Mat n m
matmul _ _ = Mat

type Expr :: Type -> Nat -> Nat -> Type
data Expr v n m where
    (:*:) :: KnownNat k => Expr v n k -> Expr v k m -> Expr v n m
    (:+:) :: Expr v n m -> Expr v n m -> Expr v n m
    One :: Expr v n n
    Zero :: Expr v n m
    Const :: Mat n m -> Expr v n m
    Var :: v -> Expr v 1 1

infixl 7 :*:
infixl 6 :+:

eval :: (KnownNat n, KnownNat m) => (v -> Mat 1 1) -> Expr v n m -> Mat n m
eval env (lhs :+: rhs) = eval env lhs `plus` eval env rhs
eval env (lhs :*: rhs) = eval env lhs `times` eval env rhs
eval env (Var v) = env v
eval _ One = Mat
eval _ Zero = Mat
eval _ (Const m) = m

forwardAD :: Eq v => v -> Expr v n m -> Expr v n m
forwardAD v (Var v') = if v == v' then One else Zero
forwardAD v (lhs :+: rhs) = forwardAD v lhs :+: forwardAD v rhs
forwardAD v (lhs :*: rhs) = forwardAD v lhs :*: rhs :+: lhs :*: forwardAD v rhs
forwardAD _ One = Zero
forwardAD _ Zero = Zero
forwardAD _ (Const _) = Zero

-- forward dg(x)/dx f -> df(g(x))/dx
-- reverse g df(g(x))/dg(x) -> df(g(x))/dx
-}

type Expr :: ((Nat, Nat) -> Type) -> (Nat, Nat) -> Type
data Expr v tup where
    (:*:) :: KnownNat k => Expr v '(n, k) -> Expr v '(k, m) -> Expr v '(n, m)
    (:+:) :: Expr v '(n, m) -> Expr v '(n, m) -> Expr v '(n, m)
    FromMat :: Mat '(n, m) -> Expr v '(n, m)
    T :: Expr v '(n, m) -> Expr v '(m, n)
    Var :: v '(n, m) -> Expr v '(n, m)

infixl 7 :*:
infixl 6 :+:

deriving instance (forall n m. Show (v '(n, m)), KnownNat a, KnownNat b) => Show (Expr v '(a, b))

instance GEq v => GEq (Expr v) where
    geq (Var lhs) (Var rhs) = geq lhs rhs
    geq (FromMat lhs) (FromMat rhs) = geq lhs rhs
    geq (T lhs) (T rhs) = do
        (Refl :: '(n, m) :~: '(n', m')) <- geq lhs rhs
        return Refl
    geq (llhs :+: lrhs) (rlhs :+: rrhs) = do
        Refl <- geq llhs rlhs
        Refl <- geq lrhs rrhs
        return Refl
    geq (llhs :*: lrhs) (rlhs :*: rrhs) = do
        Refl <- geq llhs rlhs
        Refl <- geq lrhs rrhs
        return Refl
    geq _ _ = Nothing

instance SizedSemiring (Expr v) where
    plus = (:+:)
    times = (:*:)
    tr = T
    fromMat = FromMat

eval :: (SizedSemiring d, KnownNat n, KnownNat m)
     => (forall n' m'. v '(n', m') -> d '(n', m'))
     -> Expr v '(n, m)
     -> d '(n, m)
eval env (lhs :+: rhs) = eval env lhs `plus` eval env rhs
eval env (lhs :*: rhs) = eval env lhs `times` eval env rhs
eval env (T v) = tr (eval env v)
eval env (Var v) = env v
eval _ (FromMat mat) = fromMat mat

type Var :: (Nat, Nat) -> Type
data Var tup where
    X :: Var '(4, 3)
    Y :: Var '(3, 2)

deriving instance Show (Var tup)

deriveGEq ''Var
deriveGCompare ''Var
deriveGShow ''Var
deriveArgDict ''Var

reverseADSpecialized' :: (KnownNat n, KnownNat m) => Expr Var '(n, m) -> Expr Var '(n, m) -> DMap Var (Expr Var)
reverseADSpecialized' (FromMat _) _ = DMap.empty
reverseADSpecialized' (Var v) grad = DMap.singleton v grad
reverseADSpecialized' (T expr) grad = reverseADSpecialized' expr (T grad)
reverseADSpecialized' (lhs :+: rhs) grad = DMap.unionWithKey combine (reverseADSpecialized' lhs grad) (reverseADSpecialized' rhs grad)
-- NOTE: The complexity of DMap.unionWithKey is O(n + m) for some reason. Not as efficient as it could be?
reverseADSpecialized' (lhs :*: rhs) grad = DMap.unionWithKey combine leftMap rightMap
    where leftMap = reverseADSpecialized' lhs (grad :*: T rhs)
          rightMap = reverseADSpecialized' rhs (T lhs :*: grad)

-- reverseADSpecialized :: GCompare v => Expr v '(1, 1) -> DMap v (Expr v)
reverseADSpecialized :: Expr Var '(1, 1) -> DMap Var (Expr Var)
reverseADSpecialized expr = reverseADSpecialized' expr (FromMat idMat)

testDerivSpecialized :: DMap Var (Expr Var)
testDerivSpecialized = reverseADSpecialized testExpr

type SizedModule :: ((Nat, Nat) -> Type) -> ((Nat, Nat) -> Type) -> Constraint
class (SizedSemiring d, forall tup. Monoid (e tup)) => SizedModule d e | e -> d where
    vlscale :: forall n k m. (KnownNat n, KnownNat k, KnownNat m) => d '(n, k) -> e '(k, m) -> e '(n, m)
    vrscale :: forall n k m. (KnownNat n, KnownNat k, KnownNat m) => e '(n, k) -> d '(k, m) -> e '(n, m)
    vtr :: forall n m. (KnownNat n, KnownNat m) => e '(n, m) -> e '(m, n)

type Dual :: ((Nat, Nat) -> Type) -> ((Nat, Nat) -> Type) -> (Nat, Nat) -> Type
data Dual d e tup = Dual (d tup) (e tup)

type Hom :: ((Nat, Nat) -> Type) -> Type -> (Nat, Nat) -> Type
newtype Hom d e tup = Hom (d tup -> e)

instance Semigroup e => Semigroup (Hom d e tup) where
    Hom lhs <> Hom rhs = Hom $ \grad -> lhs grad <> rhs grad

instance Monoid e => Monoid (Hom d e tup) where
    mempty = Hom $ \_ -> mempty

instance (SizedSemiring d, Monoid e) => SizedModule d (Hom d e) where
    vlscale factor (Hom f) = Hom $ \grad -> f (tr factor `times` grad)
    vrscale (Hom f) factor = Hom $ \grad -> f (grad `times` tr factor)
    vtr (Hom f) = Hom $ \grad -> f (tr grad)

instance SizedModule d e => SizedSemiring (Dual d e) where
    fromMat mat = Dual (fromMat mat) mempty
    plus (Dual f df) (Dual g dg) = Dual (f `plus` g) (df <> dg)
    times (Dual f df) (Dual g dg) = Dual (f `times` g) ((f `vlscale` dg) <> (df `vrscale` g))
    tr (Dual f df) = Dual (tr f) (vtr df)

type Sparse :: (k -> Type) -> (k -> Type) -> Type
newtype Sparse v d = Sparse (DMap v d)

-- Get rid of second parameter.
type Combine :: (k -> Type) -> (k -> Type) -> Constraint
class Combine k v where
    combine :: k tup -> v tup -> v tup -> v tup

-- TODO: Generate automatically
instance Combine Var (Expr Var) where
    combine X l r = l :+: r
    combine Y l r = l :+: r
    {-# INLINE combine #-}

instance Combine Var Mat where
    combine X l r = l `plus` r
    combine Y l r = l `plus` r
    {-# INLINE combine #-}

instance (GCompare k, Combine k v) => Semigroup (Sparse k v) where
    (Sparse lhs) <> (Sparse rhs) = Sparse $ DMap.unionWithKey combine lhs rhs

instance (GCompare k, Combine k v) => Monoid (Sparse k v) where
    mempty = Sparse DMap.empty

reverseAD :: forall v d. (GCompare v, SizedSemiring d, Combine v d)
          => (forall n' m'. v '(n', m') -> d '(n', m'))
          -> Expr v '(1, 1)
          -> DMap v d
reverseAD env expr = let Dual _ (Hom rev) = eval env' expr
                         Sparse map = rev (fromMat idMat)
                      in map
    where env' :: forall n' m'. v '(n', m') -> Dual d (Hom d (Sparse v d)) '(n', m')
          env' v = Dual (env v) (Hom $ \grad -> Sparse $ DMap.singleton v grad)

testExpr :: Expr Var '(1, 1)
testExpr = FromMat m1 :*: Var X :*: Var Y :*: FromMat m2
    where m1 :: Mat '(1, 4)
          m1 = Mat $ LinAlg.matrix [1, 2, 3, 4]

          m2 :: Mat '(2, 1)
          m2 = Mat $ LinAlg.matrix [5, 6]

testDeriv :: DMap Var (Expr Var)
testDeriv = reverseAD env testExpr
    where env :: Var tup -> Expr Var tup
          env X = Var X
          env Y = Var Y

testDerivDirect :: DMap Var Mat
testDerivDirect = reverseAD env testExpr
    where env :: Var tup -> Mat tup
          env X = Mat $ LinAlg.matrix [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
          env Y = Mat $ LinAlg.matrix [1, 2, 3, 4, 5, 6]

instance (KnownNat n, KnownNat m) => Arbitrary (Mat '(n, m)) where
    arbitrary = do list <- vector $ fromInteger (natVal (Proxy @n) * natVal (Proxy @m))
                   return $ Mat $ LinAlg.matrix list

data MatrixOps = Matmul | MatmulX | MatmulY | Add | Transpose | FromMatrix deriving (Enum, Bounded)

instance (KnownNat n, KnownNat m) => Arbitrary (Expr Var '(n, m)) where
    arbitrary = do sz <- getSize
                   (expr, _) <- generateWithSize sz
                   return expr
        where generateWithSize :: forall n' m'. (KnownNat n', KnownNat m') => Int -> Gen (Expr Var '(n', m'), Int)
              generateWithSize sz = do
                  choice <- if sz <= 0
                               then pure FromMatrix
                               else chooseEnum @MatrixOps (Matmul, Transpose)
                  case choice of
                     -- Sadly, here we have to pick a single value for k at compile time
                     Matmul     -> do (lhs :: Expr Var '(n', 3), sz')  <- generateWithSize (sz  - 1)
                                      (rhs :: Expr Var '(3, m'), sz'') <- generateWithSize (sz' - 1)
                                      return $ (lhs :*: rhs, sz'')
                     MatmulX    -> do (lhs :: Expr Var '(n', 4), sz')  <- generateWithSize (sz  - 1)
                                      (rhs :: Expr Var '(3, m'), sz'') <- generateWithSize (sz' - 2)
                                      return $ (lhs :*: Var X :*: rhs, sz'')
                     MatmulY    -> do (lhs :: Expr Var '(n', 3), sz')  <- generateWithSize (sz  - 1)
                                      (rhs :: Expr Var '(2, m'), sz'') <- generateWithSize (sz' - 2)
                                      return $ (lhs :*: Var Y :*: rhs, sz'')
                     Add        -> do (lhs, sz')  <- generateWithSize (sz  - 1)
                                      (rhs, sz'') <- generateWithSize (sz' - 1)
                                      return $ (lhs :+: rhs, sz'')
                     Transpose  -> do (expr, sz') <- generateWithSize (sz - 1)
                                      return $ (T expr, sz')
                     FromMatrix -> do mat <- arbitrary
                                      return $ (FromMat mat, sz)

exprTest :: (Expr Var '(1, 1), Mat '(4, 3), Mat '(3, 2)) -> Bool
exprTest (expr, x, y) = isJust $ do
    let dx = fromMaybe zeroMat $ DMap.lookup X m
    let dy = fromMaybe zeroMat $ DMap.lookup Y m
    let expr_dx' = fromMaybe (FromMat zeroMat) $ DMap.lookup X m'
    let expr_dy' = fromMaybe (FromMat zeroMat) $ DMap.lookup Y m'
    let dx' = eval env expr_dx'
    let dy' = eval env expr_dy'
    Refl <- dx `geq` dx'
    Refl <- dy `geq` dy'
    return Refl
    where m  = reverseAD env expr
          m' = reverseADSpecialized expr
          env :: Var tup -> Mat tup
          env X = x
          env Y = y

exprLhs :: (Expr Var '(1, 1), Mat '(4, 3), Mat '(3, 2)) -> (Mat '(4, 3), Mat '(3, 2))
exprLhs (expr, x, y) = let map = reverseAD env expr in (fromJust (DMap.lookup X map), fromJust (DMap.lookup Y map))
    where env :: Var tup -> Mat tup
          env X = x
          env Y = y

valueTest = do
    Refl <- dx `geq` dx'
    Refl <- dy `geq` dy'
    return Refl
    where dx  = eval env $ fromJust $ DMap.lookup X testDeriv
          dy  = eval env $ fromJust $ DMap.lookup Y testDeriv
          dx' = eval env $ fromJust $ DMap.lookup X testDerivSpecialized
          dy' = eval env $ fromJust $ DMap.lookup Y testDerivSpecialized
          env :: Var '(n, m) -> Mat '(n, m)
          env X = Mat $ LinAlg.matrix [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
          env Y = Mat $ LinAlg.matrix [1, 2, 3, 4, 5, 6]

valueTestDirect = do
    Refl <- dx `geq` dx'
    Refl <- dy `geq` dy'
    return Refl
    where dx  = fromJust $ DMap.lookup X testDerivDirect
          dy  = fromJust $ DMap.lookup Y testDerivDirect
          dx' = eval env $ fromJust $ DMap.lookup X testDerivSpecialized
          dy' = eval env $ fromJust $ DMap.lookup Y testDerivSpecialized
          env :: Var '(n, m) -> Mat '(n, m)
          env X = Mat $ LinAlg.matrix [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
          env Y = Mat $ LinAlg.matrix [1, 2, 3, 4, 5, 6]
