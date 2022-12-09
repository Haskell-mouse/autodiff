{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Sized.LiftInstances (module Sized.LiftInstances) where

import GHC.TypeLits (KnownNat)
import Language.Haskell.TH.Syntax (Exp, Lift (lift, liftTyped), Quote)
import Language.Haskell.TH.Syntax.Compat (Splice, unTypeSplice)
import Numeric.LinearAlgebra.Data (toLists)
import Numeric.LinearAlgebra.Static (L, Sized (extract, fromList))
import Sized (Mat (Mat))

instance (KnownNat n, KnownNat m) => Lift (L n m) where
  liftTyped :: Quote m1 => L n m -> Splice m1 (L n m)
  liftTyped t = [||fromList t'||]
    where
      t' = (concat . toLists . extract) t
  lift :: Quote m1 => L n m -> m1 Exp
  lift t = unTypeSplice (liftTyped t)

deriving instance Lift (Mat d)
