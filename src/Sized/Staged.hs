{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Sized.Staged where

-- TODO: Add export list

import GHC.TypeLits
import Data.Kind
import Language.Haskell.TH.Syntax.Compat
import Language.Haskell.TH.Syntax (Lift (liftTyped, lift), runQ)
import Data.GADT.Compare (GCompare)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import qualified Numeric.LinearAlgebra.Static as LinAlg
import Sized
import Data.Semigroup (Endo)
import Data.Monoid (Endo(..))

-- | Constrained splice.
-- 
-- Wraps a SpliceQ into a type constructor.
-- Used to implement splicing for indexed matrices.
type CSpliceQ :: (constr -> Type) -> constr -> Type
newtype CSpliceQ typ constr = CSpliceQ { splice :: SpliceQ (typ constr) }

-- TODO: Test to see if it works
instance (KnownNat n, KnownNat m) => Lift (LinAlg.L n m) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = [| x |]

deriving instance Lift (Mat d)

instance SizedSemiring a => SizedSemiring (CSpliceQ a) where
  (CSpliceQ e1) `plus` (CSpliceQ e2) = CSpliceQ [|| $$e1 `plus` $$e2 ||]
  (CSpliceQ e1) `times` (CSpliceQ e2) = CSpliceQ [|| $$e1 `times` $$e2 ||]
  tr (CSpliceQ e1) = CSpliceQ [|| tr $$e1 ||]
  fromMat mat = CSpliceQ [|| fromMat mat ||]

-- TODO: Test if this instance improves performance
-- instance SizedSemiring (CSpliceQ Mat) where
--   (CSpliceQ e1) `plus` (CSpliceQ e2) = CSpliceQ
--     [|| let (Mat lhs) = $$e1
--             (Mat rhs) = $$e2
--          in Mat $ lhs `LinAlg.add` rhs ||]
--   (CSpliceQ e1) `times` (CSpliceQ e2) = CSpliceQ
--     [|| let (Mat lhs) = $$e1
--             (Mat rhs) = $$e2
--          in Mat $ (LinAlg.<>) lhs rhs ||]
--   tr (CSpliceQ e1) = CSpliceQ
--     [|| let (Mat mat) = $$e1 in Mat $ LinAlg.tr mat ||]
--   fromMat x = CSpliceQ [|| error "TODO" ||]

reverseADStaged ::
  forall v d.
  (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup)) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)
reverseADStaged env expr =
  [|| let Dual _ (Hom rev) = $$(splice (eval env' expr))
          Sparse dmap = rev $ fromMat idMat
       in dmap ||]
  where
    env' :: forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ (Dual d (Hom d (Sparse v d))) '(n', m')
    env' v = CSpliceQ
      [|| Dual
            $$(splice (env v))
            (Hom $ \grad -> Sparse $ DMap.singleton $$(splice v) grad) ||]

reverseADEndoStaged ::
  forall v d.
  (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup)) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)
reverseADEndoStaged env expr =
  [|| let Dual _ (Hom rev) = $$(splice (eval env' expr))
          Sparse dmap = appEndo (rev $ fromMat idMat) mempty
       in dmap ||]
  where
    env' ::
      forall n' m'.
      (KnownNat n', KnownNat m') =>
      CSpliceQ v '(n', m') ->
      CSpliceQ (Dual d (Hom d (Endo (Sparse v d)))) '(n', m')
    env' v = CSpliceQ
      [|| Dual
            $$(splice (env v))
            (Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith plus $$(splice v) grad acc) ||]
