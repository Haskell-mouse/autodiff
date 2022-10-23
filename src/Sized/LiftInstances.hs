{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Sized.LiftInstances (module Sized.LiftInstances) where

import GHC.TypeLits (KnownNat)
import Language.Haskell.TH (unsafeCodeCoerce)
import Language.Haskell.TH.Syntax (Lift (lift, liftTyped))
import qualified Numeric.LinearAlgebra.Static as LinAlg
import Sized (Mat (Mat))

instance Lift a => Lift (IO a) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = [|x|]

instance (KnownNat n, KnownNat m) => Lift (LinAlg.L n m) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = [|x|]

deriving instance Lift (Mat d)
