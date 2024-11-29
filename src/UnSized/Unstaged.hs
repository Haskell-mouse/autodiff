{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, IncoherentInstances #-}

module UnSized.Unstaged where 

import GHC.TypeLits
import Data.Kind
import Language.Haskell.TH.Syntax.Compat
import Data.Semigroup (Endo)
import Data.Monoid (Endo(..))
import qualified Numeric.LinearAlgebra.Static as LinAlg
import qualified Numeric.LinearAlgebra as LinAlg (Additive(..))

import Data.Functor.Identity
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Control.Monad.IO.Class (liftIO)

import Unsafe.Coerce

import UnSized.UnSized

import Data.Map

type SCont :: Type -> Type -> Type
newtype SCont a b = Cont { runCont :: forall r. ((a, b) -> r) -> r }

-- todo 
-- turn [||Hom d e||] to Hom ([||d||] [|| e ||])
-- similar with endo

instance Semiring (SCont (Double) (Hom (Double) (Endo (Sparse k (Double)))) ) where 
    zero = Cont (\f -> f (zero,mempty))
    one = Cont (\f -> f (one,mempty))
    (Cont f1) `plus` (Cont f2) = 
        let p =  (\(pr1,(Hom df1)) (pr2,(Hom df2)) -> 
                       (pr1 + pr2, Hom (\grad -> (df1 grad) <> (df2 grad))))
        in Cont $ \k -> f1 (\x -> f2 (\y -> k (p x y)))
    (Cont f1) `times` (Cont f2) =
        let p = (\(pr1, tan1) (pr2,tan2) ->
                           let (Hom df1) = tan1
                               (Hom df2) = tan2
                           in (times pr1 pr2, Hom (\grad -> ((df2 $ pr1 `times` grad) <> (df1 (grad `times` (pr2)))) )  ))
        in Cont $ \k -> f1 (\x -> f2 (\y -> k (p x y)))

reverseADEndoStaged ::
  forall d.
  (Semiring d, Semigroup d, Show d, Num d, Semiring
                          (SCont d (Hom d (Endo (Sparse Nat d)))), 
                          Convertable Nat (SCont d (Hom d (Endo (Sparse Nat d))))) =>
  (Nat -> d) ->
  Expr d ->
  Map Nat d
reverseADEndoStaged env expr =  
  let (Cont f) = (eval env' expr)
  in  f (\(d, (sRev)) -> 
            let (Hom rev) = sRev
                sMmap = appEndo (rev $ fromNatural 1) mempty
            in let Sparse mmap = sMmap
                   in mmap)
  where
    env' :: Nat -> SCont d (Hom d (Endo (Sparse Nat d)))
    env' v = 
      let tan = Hom (\grad -> Endo $ \acc -> insertWithSparse plus v grad acc)
      in Cont (\f -> f (env v, tan))

{-
eval :: (Semiring d, Convertable v d)
     => (Nat -> d)
     -> Expr v
     -> d
-}

instance (Convertable v d, Semiring d) => Convertable v (SCont d (Hom d (Endo (Sparse Nat d)))) where 
  cast v = 
    let tan = Hom (\grad -> Endo $ \acc -> acc)
    in Cont (\f -> f (cast v, tan))

type AutoDiffStagedType =
  forall d.
  (Ord d, Semiring d, Semigroup d, Show d, Num d, Semiring
                          (SCont d (Hom d (Endo (Sparse Nat d)))),
                          Convertable Nat (SCont d (Hom d (Endo (Sparse Nat d))))
  ) =>
  (Nat -> d) ->
  (Expr d) ->
  (Map Nat d)


-- (Double -> Double) -> 
-- Expr Double -> 
-- Map Double Double