{-# LANGUAGE BangPatterns #-}
module Sized ( SizedSemiring(..)
             , Mat(..)
             , Expr(..)
             , reverseAD
             , reverseADEndo
             , idMat
             , zeroMat
             , oneMat

             , Hom (Hom)
             , Sparse (Sparse)
             , Dual (Dual)
             , eval

             -- Testing
             , reverseAD'
             , reverseAD'Endo
             , Var(..)
             , reverseADTopo
             , reverseADTopoEndo
             , SizedModule(..)
             ) where

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
import Control.DeepSeq (NFData(..))
import Data.Monoid (Endo(..))

import Data.Proxy
import Type.Reflection
import Control.Monad (guard)

import Data.Maybe (fromMaybe, fromJust, isJust)

import Test.QuickCheck
import Unsafe.Coerce
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

type Mat :: (Nat, Nat) -> Type
data Mat tup where
    Mat :: (KnownNat n, KnownNat m) => L n m -> Mat '(n, m)

deriving instance (KnownNat n, KnownNat m) => Show (Mat '(n, m))

instance NFData (Mat tup) where
    rnf (Mat m) = rnf m

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

oneMat :: (KnownNat n, KnownNat m) => Mat '(n, m)
oneMat = Mat $ LinAlg.build (\_ _ -> 1)

type SizedSemiring :: ((Nat, Nat) -> Type) -> Constraint
class SizedSemiring d where
    plus  :: (KnownNat n, KnownNat m) => d '(n, m) -> d '(n, m) -> d '(n, m)
    times :: (KnownNat n, KnownNat m, KnownNat k) => d '(n, k) -> d '(k, m) -> d '(n, m)
    tr    :: (KnownNat n, KnownNat m) => d '(n, m) -> d '(m, n)
    fromMat :: (KnownNat n, KnownNat m) => Mat '(n, m) -> d '(n, m)

infixl 7 `times`
infixl 6 `plus`

type SizedModule :: ((Nat, Nat) -> Type) -> ((Nat, Nat) -> Type) -> Constraint
class (SizedSemiring d, forall tup. Monoid (e tup)) => SizedModule d e | e -> d where
    vlscale :: forall n k m. (KnownNat n, KnownNat k, KnownNat m) => d '(n, k) -> e '(k, m) -> e '(n, m)
    vrscale :: forall n k m. (KnownNat n, KnownNat k, KnownNat m) => e '(n, k) -> d '(k, m) -> e '(n, m)
    vtr :: forall n m. (KnownNat n, KnownNat m) => e '(n, m) -> e '(m, n)

instance SizedSemiring Mat where
    (Mat lhs) `plus` (Mat rhs) = Mat $ lhs `LinAlg.add` rhs
    (Mat lhs) `times` (Mat rhs) = Mat $ (LinAlg.<>) lhs rhs
    tr (Mat mat) = Mat $ LinAlg.tr mat
    fromMat = id

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

coerceExprSize :: forall v. (forall n m. Expr v '(n, m)) -> (forall tup. Expr v tup)
coerceExprSize expr = unsafeCoerce expr

coerceExprSize' :: forall n m v. (forall tup. Expr v tup) -> Expr v '(n, m)
coerceExprSize' expr = unsafeCoerce expr

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

instance Semigroup (Expr v tup) where
    -- forall v. (forall tup. Expr v tup -> Expr v tup) ~ (forall n m. Expr v '(n, m) -> Expr v '(n, m))
    -- (<>) = unsafeCoerce (:+:)
    lhs@(_ :+: _)   <> rhs@(_ :+: _)   = lhs :+: rhs
    lhs@(_ :+: _)   <> rhs@(_ :*: _)   = lhs :+: rhs
    lhs@(_ :+: _)   <> rhs@(FromMat _) = lhs :+: rhs
    lhs@(_ :+: _)   <> rhs@(T _)       = lhs :+: rhs
    lhs@(_ :+: _)   <> rhs@(Var _)     = lhs :+: rhs
    lhs@(_ :*: _)   <> rhs@(_ :+: _)   = lhs :+: rhs
    lhs@(_ :*: _)   <> rhs@(_ :*: _)   = lhs :+: rhs
    lhs@(_ :*: _)   <> rhs@(FromMat _) = lhs :+: rhs
    lhs@(_ :*: _)   <> rhs@(T _)       = lhs :+: rhs
    lhs@(_ :*: _)   <> rhs@(Var _)     = lhs :+: rhs
    lhs@(FromMat _) <> rhs@(_ :+: _)   = lhs :+: rhs
    lhs@(FromMat _) <> rhs@(_ :*: _)   = lhs :+: rhs
    lhs@(FromMat _) <> rhs@(FromMat _) = lhs :+: rhs
    lhs@(FromMat _) <> rhs@(T _)       = lhs :+: rhs
    lhs@(FromMat _) <> rhs@(Var _)     = lhs :+: rhs
    lhs@(T _)       <> rhs@(_ :+: _)   = lhs :+: rhs
    lhs@(T _)       <> rhs@(_ :*: _)   = lhs :+: rhs
    lhs@(T _)       <> rhs@(FromMat _) = lhs :+: rhs
    lhs@(T _)       <> rhs@(T _)       = lhs :+: rhs
    lhs@(T _)       <> rhs@(Var _)     = lhs :+: rhs
    lhs@(Var _)     <> rhs@(_ :+: _)   = lhs :+: rhs
    lhs@(Var _)     <> rhs@(_ :*: _)   = lhs :+: rhs
    lhs@(Var _)     <> rhs@(FromMat _) = lhs :+: rhs
    lhs@(Var _)     <> rhs@(T _)       = lhs :+: rhs
    lhs@(Var _)     <> rhs@(Var _)     = lhs :+: rhs

eval :: (SizedSemiring d, KnownNat n, KnownNat m)
     => (forall n' m'. (KnownNat n', KnownNat m') => v '(n', m') -> d '(n', m'))
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
reverseADSpecialized' (lhs :+: rhs) grad = DMap.unionWithKey (const (<>)) (reverseADSpecialized' lhs grad) (reverseADSpecialized' rhs grad)
reverseADSpecialized' (lhs :*: rhs) grad = DMap.unionWithKey (const (<>)) leftMap rightMap
    where leftMap = reverseADSpecialized' lhs (grad :*: T rhs)
          rightMap = reverseADSpecialized' rhs (T lhs :*: grad)

-- reverseADSpecialized :: GCompare v => Expr v '(1, 1) -> DMap v (Expr v)
reverseADSpecialized :: Expr Var '(1, 1) -> DMap Var (Expr Var)
reverseADSpecialized expr = reverseADSpecialized' expr (FromMat idMat)

testDerivSpecialized :: DMap Var (Expr Var)
testDerivSpecialized = reverseADSpecialized testExpr

type Dual :: ((Nat, Nat) -> Type) -> ((Nat, Nat) -> Type) -> (Nat, Nat) -> Type
data Dual d e tup = Dual (d tup) (e tup)

instance SizedModule d e => SizedSemiring (Dual d e) where
    fromMat mat = Dual (fromMat mat) mempty
    plus (Dual f df) (Dual g dg) = Dual (f `plus` g) (df <> dg)
    times (Dual f df) (Dual g dg) = Dual (f `times` g) ((f `vlscale` dg) <> (df `vrscale` g))
    tr (Dual f df) = Dual (tr f) (vtr df)

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

type Sparse :: (k -> Type) -> (k -> Type) -> Type
newtype Sparse v d = Sparse (DMap v d)

instance Semigroup (Mat tup) where
    lhs@(Mat _) <> rhs@(Mat _) = lhs `plus` rhs

instance (GCompare k, forall tup. Semigroup (v tup)) => Semigroup (Sparse k v) where
    (Sparse lhs) <> (Sparse rhs) = Sparse $ DMap.unionWithKey (const (<>)) lhs rhs

instance (GCompare k, forall tup. Semigroup (v tup)) => Monoid (Sparse k v) where
    mempty = Sparse DMap.empty

reverseAD :: forall v d. (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup))
          => (forall n' m'. v '(n', m') -> d '(n', m'))
          -> Expr v '(1, 1)
          -> DMap v d
reverseAD env expr = let Dual _ (Hom rev) = eval env' expr
                         Sparse map = rev $ fromMat idMat
                      in map
    where env' :: forall n' m'. (KnownNat n', KnownNat m')
               => v '(n', m')
               -> Dual d (Hom d (Sparse v d)) '(n', m')
          env' v = Dual (env v) (Hom $ \grad -> Sparse $ DMap.singleton v grad)

reverseADEndo :: forall v d. (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup))
              => (forall n' m'. v '(n', m') -> d '(n', m'))
              -> Expr v '(1, 1)
              -> DMap v d
reverseADEndo env expr = let Dual _ (Hom rev) = eval env' expr
                             Sparse map = appEndo (rev $ fromMat idMat) mempty
                          in map
    where env' :: forall n' m'. (KnownNat n', KnownNat m')
               => v '(n', m')
               -> Dual d (Hom d (Endo (Sparse v d))) '(n', m')
          env' v = Dual (env v) (Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith plus v grad acc)

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

{-
type Sparse' :: (k -> Type) -> (k -> Type) -> (Nat, Nat) -> Type
newtype Sparse' v d tup = Sparse' (DMap v d)

instance (SizedSemiring d) => SizedModule d (Sparse' v d) where
    vlscale :: forall n k m. (KnownNat n, KnownNat k, KnownNat m) => d '(n, k) -> e '(k, m) -> e '(n, m)
    vlscale d (Sparse' m) = 
    vrscale :: forall n k m. (KnownNat n, KnownNat k, KnownNat m) => e '(n, k) -> d '(k, m) -> e '(n, m)
    vtr :: forall n m. (KnownNat n, KnownNat m) => e '(n, m) -> e '(m, n)

forwardAD :: forall n m v e. (KnownNat n, KnownNat m, SizedSemiring e)
          => (forall d. SizedSemiring d => DMap v d -> d '(1, 1))
          -> DMap v e
          -> DMap v e
forwardAD f x = let Dual _ (Sparse' deriv) = f dual in deriv
    where dual :: DMap v (Dual e (Sparse' v e))
          dual = DMap.mapWithKey (\v e -> Dual e $ Sparse' $ DMap.singleton v $ fromMat zeroMat) x
-}



{-
reverseAD' :: forall v d n m. (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup), KnownNat n, KnownNat m)
          => DMap v d
          -> (forall d'. SizedSemiring d' => DMap v d' -> d' '(1, 1))
          -> DMap v d
reverseAD' x f = let Dual _ (Hom rev) = f dualMap
                     Sparse map = appEndo (rev $ fromMat idMat) mempty
                  in map
    where dualMap :: DMap v (Dual d (Hom d (Endo (Sparse v d))))
          dualMap = DMap.mapWithKey (unsafeCoerce fn) x

          fn :: v '(n, m) -> d '(n, m) -> Dual d (Hom d (Endo (Sparse v d))) '(n, m)
          fn v e = Dual e $ Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith plus v grad acc
-}

{-
type TopoOrdered :: ((Nat, Nat) -> Type) -> Type
data TopoOrdered d = TopoOrdered Int d

instance SizedSemiring d => TopoOrdered d where
    TopoOrdered ord1 lhs `plus`  TopoOrdered ord2 rhs = TopoOrdered (max ord1 ord2 + 1) (lhs `plus` rhs)
    TopoOrdered ord1 lhs `times` TopoOrdered ord2 rhs = TopoOrdered (max ord1 ord2 + 1) (lhs `plus` rhs)
    tr (TopoOrdered ord d) = TopoOrdered (ord + 1) (tr d)
    -- TODO: Is this actually ok to do?
    fromMat mat = TopoOrdered (-1) mat
-}

type Opt v d = Hom d (Endo (Sparse v d))

reverseAD' :: forall v d. (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup))
           => (DMap v (Dual d (Hom d (Sparse v d))) -> Dual d (Hom d (Sparse v d)) '(1, 1))
           -> DMap v d
           -> DMap v d
reverseAD' f x = let Dual _ (Hom rev) = f dualMap
                     Sparse map = rev $ fromMat idMat
                  in map
    where dualMap :: DMap v (Dual d (Hom d (Sparse v d)))
          dualMap = DMap.mapWithKey (\k v -> Dual v $ Hom $ \grad -> Sparse $ DMap.singleton k grad) x

           -- => (forall d'. SizedSemiring d' => DMap v d' -> d' '(1, 1))
reverseAD'Endo :: forall v d. (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup))
               => (DMap v (Dual d (Opt v d)) -> Dual d (Opt v d) '(1, 1))
               -> DMap v d
               -> DMap v d
reverseAD'Endo f x = let Dual _ (Hom rev) = f dualMap
                         Sparse map = appEndo (rev $ fromMat idMat) mempty
                  in map
    where dualMap :: DMap v (Dual d (Opt v d))
          dualMap = DMap.mapWithKey (\k v ->
              Dual v $ Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith (<>) k grad acc)
              x
          -- I would really like to use `flip DMap.mapWithKey x $ \(..) -> {- lambda -}`, however
          -- for some reason it won't compile.

type TopoOrdered :: ((Nat, Nat) -> Type) -> (Nat, Nat) -> Type
data TopoOrdered d tup = TopoOrdered Int (d tup)

instance SizedSemiring d => SizedSemiring (TopoOrdered d) where
    TopoOrdered ord1 lhs `plus`  TopoOrdered ord2 rhs = TopoOrdered (max ord1 ord2 + 1) (lhs `plus` rhs)
    TopoOrdered ord1 lhs `times` TopoOrdered ord2 rhs = TopoOrdered (max ord1 ord2 + 1) (lhs `times` rhs)
    tr (TopoOrdered ord d) = TopoOrdered (ord + 1) (tr d)
    -- TODO: Is this actually ok to do?
    fromMat mat = TopoOrdered 0 (fromMat mat)

type Hom' :: ((Nat, Nat) -> Type) -> Type -> (Nat, Nat) -> Type
newtype Hom' d e tup = Hom' (d tup -> e)

instance Semigroup e => Semigroup (Hom' d e tup) where
    Hom' lhs <> Hom' rhs = Hom' $ \grad -> lhs grad <> rhs grad

instance Monoid e => Monoid (Hom' d e tup) where
    mempty = Hom' $ \_ -> mempty

instance (SizedSemiring d, Monoid e) => SizedModule d (Hom' (TopoOrdered d) e) where
    vlscale factor (Hom' f) = Hom' $ \grad -> f (TopoOrdered 0 (tr factor) `times` grad)
    vrscale (Hom' f) factor = Hom' $ \grad -> f (grad `times` TopoOrdered 0 (tr factor))
    vtr (Hom' f) = Hom' $ \grad -> f (tr grad)

reverseADTopo :: forall v d. (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup))
              => (DMap v (Dual d (Hom' (TopoOrdered d) (Sparse v d))) -> Dual d (Hom' (TopoOrdered d) (Sparse v d)) '(1, 1))
              -> DMap v d
              -> DMap v d
reverseADTopo f x = let Dual _ (Hom' rev) = f dualMap
                        Sparse map = rev $ TopoOrdered 0 (fromMat idMat)
                     in map
    where dualMap :: DMap v (Dual d (Hom' (TopoOrdered d) (Sparse v d)))
          dualMap = DMap.mapWithKey (\k v ->
              Dual v $ Hom' $ let !memo = unsafePerformIO $ newIORef @[(Int, Sparse v d)] []

                                  fn (TopoOrdered i grad) = Sparse $ DMap.singleton k grad

                                  memoized (TopoOrdered i grad) = unsafePerformIO $ do
                                      list <- readIORef memo
                                      case lookup i list of
                                        Just v -> return v
                                        Nothing -> do let res = fn (TopoOrdered i grad)
                                                      modifyIORef' memo (\l -> (i, res):l)
                                                      return res
                                  {-# NOINLINE memoized #-}
                               in memoized)
              x

          getClosureId :: IORef Int -> Int
          getClosureId ref = unsafePerformIO $ do i <- readIORef ref
                                                  modifyIORef' ref (+1)
                                                  return i

type Opt' v d = Hom' (TopoOrdered d) (Endo (Sparse v d))

reverseADTopoEndo :: forall v d. (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup))
                  => (DMap v (Dual d (Opt' v d)) -> Dual d (Opt' v d) '(1, 1))
                  -> DMap v d
                  -> DMap v d
reverseADTopoEndo f x = let Dual _ (Hom' rev) = f dualMap
                            Sparse map = appEndo (rev $ TopoOrdered 0 (fromMat idMat)) mempty
                     in map
    where dualMap :: DMap v (Dual d (Opt' v d))
          dualMap = DMap.mapWithKey (\k v ->
              Dual v $ Hom' $ let !memo = unsafePerformIO $ newIORef @[(Int, Endo (Sparse v d))] []

                                  fn (TopoOrdered i grad) = Endo $
                                      \(Sparse acc) -> Sparse $ DMap.insertWith (<>) k grad acc

                                  memoized (TopoOrdered i grad) = unsafePerformIO $ do
                                      list <- readIORef memo
                                      case lookup i list of
                                        Just v -> return v
                                        Nothing -> do let res = fn (TopoOrdered i grad)
                                                      modifyIORef' memo (\l -> (i, res):l)
                                                      return res
                                  {-# NOINLINE memoized #-}
                               in memoized)
              x

          getClosureId :: IORef Int -> Int
          getClosureId ref = unsafePerformIO $ do i <- readIORef ref
                                                  modifyIORef' ref (+1)
                                                  return i

{-
reverseADLens :: forall (u :: ((Nat, Nat) -> Type) -> Type) d. (SizedSemiring d, forall tup. Semigroup (d tup))
              => (forall e. SizedModule d e => u (Dual d e) -> Dual d e '(1, 1))
              -> u d
              -> u d
reverseADLens f x = let Dual _ (Hom rev) = f dualMap
                        Sparse map = appEndo (rev $ fromMat idMat) mempty
                  in map
    where dualMap :: u (Dual d (Hom d (Endo (u d))))
          dualMap = DMap.mapWithKey (\k v ->
              Dual v $ Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith (<>) k grad acc)
              x
          -- I would really like to use `flip DMap.mapWithKey x $ \(..) -> {- lambda -}`, however
          -- for some reason it won't compile.
-}
