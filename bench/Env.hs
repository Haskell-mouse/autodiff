{-# LANGUAGE DeriveLift #-}

module Env (module Env) where

import Prelude hiding (sum)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import qualified Numeric.LinearAlgebra.Static as LinAlg
import Numeric.LinearAlgebra.Static.Backprop
import Numeric.Backprop
import Lens.Micro.TH (makeLenses)
import Control.DeepSeq (NFData)
import Data.Proxy (Proxy(Proxy))
import Test.QuickCheck (generate, Arbitrary(arbitrary))
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.GADT.Compare (GCompare)
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.Kind (Type)
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (DMap, (!))
import qualified Data.Dependent.Map as DMap
import Language.Haskell.TH.Syntax (Lift)
import Sized
import Sized.Staged
import Sized.LiftInstances ()

randomL :: forall n m. (KnownNat n, KnownNat m) => IO (L n m)
randomL = do let n = natVal (Proxy @n)
             let m = natVal (Proxy @m)
             values <- sequence $ flip unfoldr 0 $ \i ->
                 if i < n * m
                    then Just (generate arbitrary :: IO Double, i + 1)
                    else Nothing
             return $ LinAlg.matrix values

randomR :: forall n. KnownNat n => IO (R n)
randomR = randomL >>= (return . LinAlg.uncol)

data BackpropNet = BackpropNet { _bpnWeights1 :: L 200 784
                               , _bpnBias1    :: R 200
                               , _bpnWeights2 :: L 150 200
                               , _bpnBias2    :: R 150
                               , _bpnWeights3 :: L 100 150
                               , _bpnBias3    :: R 100
                               , _bpnWeights4 :: L 10 100
                               , _bpnBias4    :: R 10
                               }
                               deriving (Show, Generic)

instance Backprop BackpropNet
instance NFData BackpropNet

makeLenses ''BackpropNet

backpropTest :: forall s. Reifies s W => BVar s BackpropNet -> BVar s (R 784) -> BVar s Double
backpropTest net input = layer4Out <.> one layer4Out
    where layer4Out = (net ^^. bpnWeights4) #> layer3Out + (net ^^. bpnBias4)
          layer3Out = (net ^^. bpnWeights3) #> layer2Out + (net ^^. bpnBias3)
          layer2Out = (net ^^. bpnWeights2) #> layer1Out + (net ^^. bpnBias2)
          layer1Out = (net ^^. bpnWeights1) #> input     + (net ^^. bpnBias1)

backpropDupTest :: forall s. Reifies s W => BVar s BackpropNet -> BVar s (R 784) -> BVar s Double
backpropDupTest net input = let res = layer4Out <.> one layer4Out in res + res
    where layer4Out = (net ^^. bpnWeights4) #> layer3Out + (net ^^. bpnBias4)
          layer3Out = (net ^^. bpnWeights3) #> layer2Out + (net ^^. bpnBias3)
          layer2Out = (net ^^. bpnWeights2) #> layer1Out + (net ^^. bpnBias2)
          layer1Out = (net ^^. bpnWeights1) #> input     + (net ^^. bpnBias1)

backpropTripTest :: forall s. Reifies s W => BVar s BackpropNet -> BVar s (R 784) -> BVar s Double
backpropTripTest net input = let res = layer4Out <.> one layer4Out in res + res + res
    where layer4Out = (net ^^. bpnWeights4) #> layer3Out + (net ^^. bpnBias4)
          layer3Out = (net ^^. bpnWeights3) #> layer2Out + (net ^^. bpnBias3)
          layer2Out = (net ^^. bpnWeights2) #> layer1Out + (net ^^. bpnBias2)
          layer1Out = (net ^^. bpnWeights1) #> input     + (net ^^. bpnBias1)


setupBackpropEnv :: IO (BackpropNet, R 784)
setupBackpropEnv = do w1 <- randomL
                      b1 <- randomR
                      w2 <- randomL
                      b2 <- randomR
                      w3 <- randomL
                      b3 <- randomR
                      w4 <- randomL
                      b4 <- randomR
                      input <- randomR
                      return (BackpropNet w1 b1 w2 b2 w3 b3 w4 b4, input)

type SizedNet :: ((Nat, Nat) -> Type) -> Type
data SizedNet d = SizedNet { szWeights1 :: d '(200, 784)
                           , szBias1    :: d '(200, 1)
                           , szWeights2 :: d '(150, 200)
                           , szBias2    :: d '(150, 1)
                           , szWeights3 :: d '(100, 150)
                           , szBias3    :: d '(100, 1)
                           , szWeights4 :: d '(10, 100)
                           , szBias4    :: d '(10, 1)
                           }
                           deriving Generic

deriving instance (forall n m. (KnownNat n, KnownNat m) => Show (d '(n, m))) => Show (SizedNet d)
deriving instance (forall n m. (KnownNat n, KnownNat m) => Lift (d '(n, m))) => Lift (SizedNet d)
instance (forall tup. NFData (d tup)) => NFData (SizedNet d)

type SizedNetVar :: (Nat, Nat) -> Type
data SizedNetVar tup where
    SzWeights1 :: SizedNetVar '(200, 784)
    SzBias1    :: SizedNetVar '(200, 1)
    SzWeights2 :: SizedNetVar '(150, 200)
    SzBias2    :: SizedNetVar '(150, 1)
    SzWeights3 :: SizedNetVar '(100, 150)
    SzBias3    :: SizedNetVar '(100, 1)
    SzWeights4 :: SizedNetVar '(10, 100)
    SzBias4    :: SizedNetVar '(10, 1)

deriving instance Show (SizedNetVar tup)

deriveGEq ''SizedNetVar
deriveGCompare ''SizedNetVar

exprSizedNet :: SizedNet (Expr SizedNetVar)
exprSizedNet = SizedNet { szWeights1 = Var SzWeights1
                        , szBias1    = Var SzBias1
                        , szWeights2 = Var SzWeights2
                        , szBias2    = Var SzBias2
                        , szWeights3 = Var SzWeights3
                        , szBias3    = Var SzBias3
                        , szWeights4 = Var SzWeights4
                        , szBias4    = Var SzBias4
                        }

exprSizedNetStaged :: SizedNet (Expr (CSpliceQ SizedNetVar))
exprSizedNetStaged = SizedNet { szWeights1 = Var (CSpliceQ [||SzWeights1||])
                              , szBias1    = Var (CSpliceQ [||SzBias1||])
                              , szWeights2 = Var (CSpliceQ [||SzWeights2||])
                              , szBias2    = Var (CSpliceQ [||SzBias2||])
                              , szWeights3 = Var (CSpliceQ [||SzWeights3||])
                              , szBias3    = Var (CSpliceQ [||SzBias3||])
                              , szWeights4 = Var (CSpliceQ [||SzWeights4||])
                              , szBias4    = Var (CSpliceQ [||SzBias4||])
                              }

sizedTest :: SizedSemiring d => SizedNet d -> d '(784, 1) -> d '(1, 1)
sizedTest SizedNet{..} input = fromMat oneMat `times` layer4Out
    where layer4Out = szWeights4 `times` layer3Out `plus` szBias4
          layer3Out = szWeights3 `times` layer2Out `plus` szBias3
          layer2Out = szWeights2 `times` layer1Out `plus` szBias2
          layer1Out = szWeights1 `times` input     `plus` szBias1

sizedDupTest :: SizedSemiring d => SizedNet d -> d '(784, 1) -> d '(1, 1)
sizedDupTest SizedNet{..} input = let res = fromMat oneMat `times` layer4Out in res `plus` res
    where layer4Out = szWeights4 `times` layer3Out `plus` szBias4
          layer3Out = szWeights3 `times` layer2Out `plus` szBias3
          layer2Out = szWeights2 `times` layer1Out `plus` szBias2
          layer1Out = szWeights1 `times` input     `plus` szBias1

sizedDMapTest :: SizedSemiring d => DMap SizedNetVar d -> d '(784, 1) -> d '(1, 1)
sizedDMapTest dmap input = fromMat oneMat `times` layer4Out
    where layer4Out = dmap!SzWeights4 `times` layer3Out `plus` dmap!SzBias4
          layer3Out = dmap!SzWeights3 `times` layer2Out `plus` dmap!SzBias3
          layer2Out = dmap!SzWeights2 `times` layer1Out `plus` dmap!SzBias2
          layer1Out = dmap!SzWeights1 `times` input     `plus` dmap!SzBias1

sizedDMapDupTest :: SizedSemiring d => DMap SizedNetVar d -> d '(784, 1) -> d '(1, 1)
sizedDMapDupTest dmap input = let res = fromMat oneMat `times` layer4Out in res `plus` res
    where layer4Out = dmap!SzWeights4 `times` layer3Out `plus` dmap!SzBias4
          layer3Out = dmap!SzWeights3 `times` layer2Out `plus` dmap!SzBias3
          layer2Out = dmap!SzWeights2 `times` layer1Out `plus` dmap!SzBias2
          layer1Out = dmap!SzWeights1 `times` input     `plus` dmap!SzBias1

sizedDMapTripTest :: SizedSemiring d => DMap SizedNetVar d -> d '(784, 1) -> d '(1, 1)
sizedDMapTripTest dmap input = let res = fromMat oneMat `times` layer4Out in res `plus` res `plus` res
    where layer4Out = dmap!SzWeights4 `times` layer3Out `plus` dmap!SzBias4
          layer3Out = dmap!SzWeights3 `times` layer2Out `plus` dmap!SzBias3
          layer2Out = dmap!SzWeights2 `times` layer1Out `plus` dmap!SzBias2
          layer1Out = dmap!SzWeights1 `times` input     `plus` dmap!SzBias1

exprSizedTest :: Mat '(784, 1) -> Expr SizedNetVar '(1, 1)
exprSizedTest input = sizedTest exprSizedNet (fromMat input)

exprSizedTestStaged :: Mat '(784, 1) -> Expr (CSpliceQ SizedNetVar) '(1, 1)
exprSizedTestStaged input = sizedTest exprSizedNetStaged (fromMat input)

exprSizedDupTest :: Mat '(784, 1) -> Expr SizedNetVar '(1, 1)
exprSizedDupTest input = sizedDupTest exprSizedNet (fromMat input)

setupSizedEnv :: IO (SizedNet Mat, Mat '(784, 1))
setupSizedEnv = do w1 <- randomL
                   b1 <- randomL
                   w2 <- randomL
                   b2 <- randomL
                   w3 <- randomL
                   b3 <- randomL
                   w4 <- randomL
                   b4 <- randomL
                   input <- randomL
                   return ( SizedNet { szWeights1 = Mat w1
                                     , szBias1    = Mat b1
                                     , szWeights2 = Mat w2
                                     , szBias2    = Mat b2
                                     , szWeights3 = Mat w3
                                     , szBias3    = Mat b3
                                     , szWeights4 = Mat w4
                                     , szBias4    = Mat b4
                                     }
                          , Mat input
                          )

setupSizedNet :: IO (SizedNet Mat)
setupSizedNet = do w1 <- randomL
                   b1 <- randomL
                   w2 <- randomL
                   b2 <- randomL
                   w3 <- randomL
                   b3 <- randomL
                   w4 <- randomL
                   b4 <- randomL
                   return SizedNet { szWeights1 = Mat w1
                                   , szBias1    = Mat b1
                                   , szWeights2 = Mat w2
                                   , szBias2    = Mat b2
                                   , szWeights3 = Mat w3
                                   , szBias3    = Mat b3
                                   , szWeights4 = Mat w4
                                   , szBias4    = Mat b4
                                   }

setupSizedInput :: IO (Mat '(784, 1))
setupSizedInput = Mat <$> randomL

var2Getter :: SizedNetVar '(n, m) -> SizedNet d -> d '(n, m)
var2Getter SzWeights1 = szWeights1
var2Getter SzBias1    = szBias1
var2Getter SzWeights2 = szWeights2
var2Getter SzBias2    = szBias2
var2Getter SzWeights3 = szWeights3
var2Getter SzBias3    = szBias3
var2Getter SzWeights4 = szWeights4
var2Getter SzBias4    = szBias4

var2GetterStaged ::
    (forall n' m'. (KnownNat n', KnownNat m') => Lift (d '(n', m'))) =>
    CSpliceQ SizedNetVar '(n, m) -> SizedNet d -> CSpliceQ d '(n, m)
var2GetterStaged var net = CSpliceQ [|| var2Getter $$(splice var) net ||]

lookupGrad :: forall v n m. (GCompare v, KnownNat n, KnownNat m) => DMap v Mat -> v '(n, m) -> Mat '(n, m)
lookupGrad grads v = fromMaybe zeroMat (DMap.lookup v grads)

netFromMap :: DMap SizedNetVar Mat -> SizedNet Mat
netFromMap grads = SizedNet { szWeights1 = lookupGrad grads SzWeights1
                            , szBias1    = lookupGrad grads SzBias1
                            , szWeights2 = lookupGrad grads SzWeights2
                            , szBias2    = lookupGrad grads SzBias2
                            , szWeights3 = lookupGrad grads SzWeights3
                            , szBias3    = lookupGrad grads SzBias3
                            , szWeights4 = lookupGrad grads SzWeights4
                            , szBias4    = lookupGrad grads SzBias4
                            }

mapFromNet :: SizedNet Mat -> DMap SizedNetVar Mat
mapFromNet net = DMap.fromList [ SzWeights1 :=> szWeights1 net
                               , SzBias1    :=> szBias1    net
                               , SzWeights2 :=> szWeights2 net
                               , SzBias2    :=> szBias2    net
                               , SzWeights3 :=> szWeights3 net
                               , SzBias3    :=> szBias3    net
                               , SzWeights4 :=> szWeights4 net
                               , SzBias4    :=> szBias4    net
                               ]
