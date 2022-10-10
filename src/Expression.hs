{-# LANGUAGE TemplateHaskell #-}

module Expression where

import qualified Data.Kind as Kind
import GHC.TypeLits
import Data.Type.Equality
import Data.GADT.Compare (GCompare(..), GOrdering(..), GEq(..))
import Type.Reflection
import Language.Haskell.TH.Syntax
import qualified Numeric.LinearAlgebra as LinAlg (toLists)
import qualified Numeric.LinearAlgebra.Static as LinAlg
import Data.Proxy
import Data.GADT.Show.TH (deriveGShow)
import Data.Constraint.Extras (ArgDict(..))
import Data.Constraint

import Sized

{-
data Expr v = Var v
            | FromInteger Integer
            | Negate (Expr v)
            | Expr v :+: Expr v
            | Expr v :*: Expr v
            | Expr v :/: Expr v
            deriving Show

infixl 5 :*:
infixl 4 :+:

instance Num (Expr v) where
    (+) = (:+:)
    (*) = (:*:)
    negate = Negate
    fromInteger = FromInteger

    signum _ = undefined
    abs _ = undefined

gen :: (Quote m, Show v) => Expr v -> m Exp
gen (lhs :+: rhs) = [| $(gen lhs) + $(gen rhs) |]
gen (lhs :*: rhs) = [| $(gen lhs) + $(gen rhs) |]
gen (lhs :/: rhs) = [| $(gen lhs) / $(gen rhs) |]
gen (Var v) = return $ VarE $ mkName $ show v
gen (Negate e) = [| negate $(gen e) |]
gen (FromInteger i) = [| fromInteger $lit |]
    where lit = pure $ LitE $ IntegerL i
-}

getSizes :: forall n m proxy. (KnownNat n, KnownNat m) => proxy '(n, m) -> (Integer, Integer)
getSizes _ = (natVal (Proxy @n), natVal (Proxy @m))

type Arg :: (Nat, Nat) -> Kind.Type
data Arg tup where
    Arg :: (KnownNat n, KnownNat m) => Arg '(n, m)

deriving instance Show (Arg tup)

instance GEq Arg where
    (lhs@Arg) `geq` (rhs@Arg) = do GEQ <- pure $ lhs `gcompare` rhs
                                   return Refl

instance GCompare Arg where
    (lhs@Arg) `gcompare` (rhs@Arg) = compareSizes lhs rhs
        where compareSizes :: forall n m r p proxy. (KnownNat n, KnownNat m, KnownNat r, KnownNat p)
                           => proxy '(n, m)
                           -> proxy '(r, p)
                           -> GOrdering '(n, m) '(r, p)
              compareSizes _ _ = case typeOf (Proxy :: Proxy n) `gcompare` typeOf (Proxy :: Proxy r) of
                                   GLT -> GLT
                                   GGT -> GGT
                                   GEQ -> case typeOf (Proxy :: Proxy m) `gcompare` typeOf (Proxy :: Proxy p) of
                                            GLT -> GLT
                                            GGT -> GGT
                                            GEQ -> GEQ

-- instance forall tup n m c. tup ~ '(n, m) => ArgDict (c :: tup -> Constraint) Arg where
--     argDict fa = Dict

deriveGShow ''Arg

{-
genFnExpr :: forall n r m. Quote m => String -> (forall d. SizedSemiring d => d '(n, r) -> d '(1, 1)) -> m [Dec]
genFnExpr genFn fn = do x <- newName "x"
                        let xpat = pure $ VarP x
                        [d| testFn' :: Mat '($nLit, $mLit) -> Mat '(1, 1)
                            testFn' $xpat = $(genBody x expr) |]
    where genFnName = mkName genFn
          expr = fn (Var Arg)
          nLit = pure $ LitT (NumTyLit n)
          mLit = pure $ LitT (NumTyLit m)
          (n, m) = getSizes expr
-}

genBody' :: Quote m => String -> Expr Arg tup -> m Exp
genBody' arg expr = genBody (mkName arg) expr

genBody :: Quote m => Name -> Expr Arg tup -> m Exp
genBody arg (lhs :+: rhs) = [| $(genBody arg lhs) `plus` $(genBody arg rhs) |]
genBody arg (lhs :*: rhs) = [| $(genBody arg lhs) `times` $(genBody arg rhs) |]
-- genBody (lhs :/: rhs) = [| $(genBody lhs) / $(genBody rhs) |]
genBody arg (Var Arg) = return $ VarE arg
genBody arg (T expr) = [| tr $(genBody arg expr)|]
-- genBody (Negate e) = [| negate $(genBody e) |]
genBody arg (FromMat (Mat mat)) = [| (Mat (LinAlg.matrix $lit) :: Mat '($nLit, $mLit)) |]
    where lit = pure $ ListE $ fmap (LitE . RationalL . toRational) elems
          elems = concat $ LinAlg.toLists mat'
          nLit = pure $ LitT (NumTyLit n)
          mLit = pure $ LitT (NumTyLit m)
          (n, m) = getSizes (Mat mat)
          mat' = LinAlg.extract mat
