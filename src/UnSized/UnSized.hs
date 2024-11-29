{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs    #-}

module UnSized.UnSized where 

import GHC.TypeLits
import Data.Kind
import Data.Map
import Data.Constraint.Extras.TH (deriveArgDict)
import Language.Haskell.TH.Syntax.Compat
import Control.DeepSeq (NFData(..))
import Data.Monoid (Endo(..))

import Control.Monad (guard)

import Data.Maybe (fromMaybe, fromJust, isJust)

import Test.QuickCheck
import Unsafe.Coerce
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Control.Monad.IO.Class (liftIO)
import Unsafe.Coerce


--import Data.Semiring


class (Semiring d, Monoid e) => Module d e | e -> d where 
    vscale :: d -> e -> e

class Semiring a where
    zero :: a
    one :: a
    plus :: a -> a -> a
    times :: a -> a -> a

fromNatural :: Semiring a => Natural -> a
fromNatural 0 = zero 
fromNatural x = one `plus` (fromNatural (x - 1))
-- | The class of semirings with an additive inverse.
--
--     @'negate' a '+' a = 'zero'@

class Semiring a => Ring a where
  negate :: a -> a


type Expr :: Type -> Type
data Expr v where
    (:*:) :: Expr v -> Expr v -> Expr v
    (:+:) :: Expr v -> Expr v -> Expr v
    Var :: Nat -> Expr v
    Const :: Semiring v => v -> Expr v

infixl 7 :*:
infixl 6 :+:

deriving instance (Show v) => Show (Expr v)

data SExpr v where 
    (::*::) :: SExpr v -> SExpr v -> SExpr v
    (::+::) :: SExpr v -> SExpr v -> SExpr v
    Val :: (Semiring v) => v -> SExpr v
    SVar :: Nat -> SExpr v -- get rid of SpliceQ, move code generation to eval function.
    LVar :: Int -> SExpr v 
    LetBind :: [(Int,SExpr v)] -> SExpr v -> SExpr v

deriving instance (Show v) => Show (SExpr v)
deriving instance (Eq v) => Eq (SExpr v)

instance Semiring v => Semiring (Expr v) where
    plus = (:+:)
    times = (:*:)
    zero = Const zero
    one = Const one

instance Semiring v => Semiring (SExpr v) where
    plus = (::+::)
    times = (::*::)
    zero = Val zero
    one = Val one

instance Semigroup (Expr v) where
    lhs <> rhs = lhs :+: rhs

instance Semigroup (SExpr v) where
    lhs@(_ ::+:: _)   <> rhs@(_ ::+:: _)   = lhs ::+:: rhs
    lhs@(_ ::+:: _)   <> rhs@(_ ::*:: _)   = lhs ::+:: rhs
    lhs@(_ ::+:: _)   <> rhs@(Val _)     = lhs ::+:: rhs
    lhs@(_ ::*:: _)   <> rhs@(_ ::+:: _)   = lhs ::+:: rhs
    lhs@(_ ::*:: _)   <> rhs@(_ ::+:: _)   = lhs ::+:: rhs
    lhs@(_ ::*:: _)   <> rhs@(Val _)     = lhs ::+:: rhs
    lhs@(Val _)     <> rhs@(_ ::+:: _)   = lhs ::+:: rhs
    lhs@(Val _)     <> rhs@(_ ::*:: _)   = lhs ::+:: rhs
    lhs@(Val _)     <> rhs@(Val _)     = lhs ::+:: rhs
    lhs@(SVar _)     <> rhs@(SVar _)     = lhs ::+:: rhs
    lhs@(SVar _)     <> rhs@(_ ::+:: _)   = lhs ::+:: rhs
    lhs@(SVar _)     <> rhs@(_ ::*:: _)   = lhs ::+:: rhs
    lhs@(SVar _)     <> rhs@(Val _)     = lhs ::+:: rhs
    lhs <> rhs = lhs ::+:: rhs


eval :: (Semiring d, Convertable v d)
     => (Nat -> d)
     -> Expr v
     -> d
eval env (lhs :+: rhs) = eval env lhs `plus` eval env rhs
eval env (lhs :*: rhs) = eval env lhs `times` eval env rhs
eval env (Var v) = env v
eval env (Const c) = cast c

eval'' :: (Semiring d, TH.Lift (v -> d))
      => (v -> d)
      -> Expr (SpliceQ v)
      -> SpliceQ d
eval'' env (lhs :+: rhs) = eval'' env lhs `plus` eval'' env rhs
eval'' env (lhs :*: rhs) = eval'' env lhs `times` eval'' env rhs
eval'' env (Var v)       = undefined
eval'' env (Const c)     = [|| env $$c ||]

type family PossArg a b :: Bool where 
    PossArg a a = True 
    PossArg (a -> b) _ = True 
    PossArg _ _ = False 

class Instr a r where 
    instr :: Nat -> a -> r

instance forall a b r c.(a ~ SExpr r, Instr b c) => Instr (a -> b) c where
    instr n f = instr (n+1) (f (SVar n))

instance forall a f. (a ~ SExpr f) => Instr a a where
    instr _ f = f

class Instr' a r where 
    instr' :: Nat -> a -> r

instance forall a b r c.(a ~ Expr r, Instr' b c) => Instr' (a -> b) c where
    instr' n f = instr' (n+1) (f (Var n))

instance forall a f. (a ~ Expr f) => Instr' a a where
    instr' _ f = f

class Convertable a b where 
    cast :: a -> b 

instance forall a b. (a ~ b) => Convertable a b where 
    cast a = a 

eval' :: (Semiring d, Convertable v d)
     => (Nat -> d)
     -> SExpr v
     -> d
eval' env (lhs ::+:: rhs) = eval' env lhs `plus` eval' env rhs
eval' env (lhs ::*:: rhs) = eval' env lhs `times` eval' env rhs
eval' env (SVar v) = env v
eval' env (Val a) = cast a


type Dual :: Type -> Type -> Type
data Dual d e = Dual d e

instance Module d e => Semiring (Dual d e) where
    plus (Dual f df) (Dual g dg) = Dual (f `plus` g) (df <> dg)
    times (Dual f df) (Dual g dg) = Dual (f `times` g) ((f `vscale` dg) <> (g `vscale` df))
    one = Dual one mempty
    zero = Dual zero mempty

type Hom :: Type -> Type -> Type
newtype Hom d e = Hom (d -> e)

instance Semigroup e => Semigroup (Hom d e) where
    Hom lhs <> Hom rhs = Hom $ \grad -> lhs grad <> rhs grad

instance Monoid e => Monoid (Hom d e) where
    mempty = Hom $ \_ -> mempty

instance (Semiring d, Monoid e) => Module d (Hom d e) where
    vscale factor (Hom f) = Hom $ \grad -> f (factor `times` grad)

type Sparse :: Type -> Type -> Type
newtype Sparse v d = Sparse (Map v d)

instance (Ord k, Semigroup v) => Semigroup (Sparse k v) where
    (Sparse lhs) <> (Sparse rhs) = Sparse $ unionWithKey (const (<>)) lhs rhs

instance (Ord k, Semigroup v) => Monoid (Sparse k v) where
    mempty = Sparse Data.Map.empty

insertWithSparse :: Ord k => (v -> v -> v) -> k -> v -> Sparse k v -> Sparse k v
insertWithSparse f k v (Sparse m) = Sparse (insertWith f k v m)

instance Semiring Int where
    plus = (+)
    times = (*)
    one = 1
    zero = 0

instance Semiring Double where
    plus = (+)
    times = (*)
    one = 1
    zero = 0

instance Semiring d => Semiring (SpliceQ d) where 
    plus x y = [|| $$x `plus` $$y ||]
    times x y = [|| $$x `times` $$y ||]
    one = [|| one ||]
    zero = [|| zero ||]

{-instance {-# OVERLAP #-} Semiring (SpliceQ Double) where 
    plus x y = x `optPlus` y
    times x y = x `optMult` y
    one = [|| one ||]
    zero = [|| zero ||]
-}

optPlus :: SpliceQ Double -> SpliceQ Double -> SpliceQ Double 
optPlus f s = 
  let x = [|| $$f ||]
      y = [|| $$s ||]
      z =  do 
            spX <- TH.runQ $ examineSplice f-- :: TH.Q TH.TExp
            spY <- TH.runQ $ examineSplice s-- :: TH.Q TH.TExp
            let uSpX = TH.unType spX 
                uSpY = TH.unType spY 
            case (uSpX, uSpX) of 
              (TH.LitE (TH.DoublePrimL t), _) | fromRational t == (zero :: Double) -> return spY
              (_, TH.LitE (TH.DoublePrimL t)) | fromRational t == (zero :: Double) -> return spX
              _ ->  liftIO (appendFile "SplicePlusFirst1.txt" ((show uSpX) ++ "\n")) >> examineSplice [|| $$f + $$s ||]
  in TH.liftCode z
     

optMult :: SpliceQ Double -> SpliceQ Double -> SpliceQ Double 
optMult f s = 
  let x = [|| $$f ||]
      y = [|| $$s ||]
      z =  do 
            spX <-  examineSplice f-- :: TH.Q TH.TExp
            spY <-  examineSplice s-- :: TH.Q TH.TExp
            let uSpX = TH.unType spX 
                uSpY = TH.unType spY 
            case (uSpX, uSpX) of 
              (TH.LitE (TH.DoublePrimL t), _) | fromRational t == (zero :: Double) -> return spX
              (_, TH.LitE (TH.DoublePrimL t)) | fromRational t == (zero :: Double) -> return spY
              (TH.LitE (TH.DoublePrimL t), _) | fromRational t == (one :: Double) -> return spY
              (_, TH.LitE (TH.DoublePrimL t)) | fromRational t == (one :: Double) -> return spX
              _ ->  liftIO (appendFile "SplicePlusFirst1.txt" ((show uSpX) ++ "\n")) >> (examineSplice [|| $$f * $$s ||])
  in TH.liftCode z

instance Semigroup Double where 
    (<>) = (Prelude.+)

instance Semigroup d => Semigroup (SpliceQ d) where 
    rhs <> lhs = 
        [|| let r = $$rhs
                l = $$lhs 
            in r <> l ||]

instance Monoid d => Monoid (SpliceQ d) where 
    mempty = [|| mempty ||]


f :: forall a. a -> SpliceQ a 
f x = [|| undefined :: a ||]

g123 :: SpliceQ Bool
g123 = f True 

checkOpt :: SExpr (SpliceQ Double) -> SpliceQ Double
checkOpt (Val d) = d 
checkOpt (r ::+:: l) = optPlus (checkOpt r) (checkOpt l)
checkOpt (r ::*:: l) = optMult (checkOpt r) (checkOpt l)
