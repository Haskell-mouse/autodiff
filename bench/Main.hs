module Main (main) where

import Criterion.Main

import Prelude hiding (sum)
import GHC.Generics (Generic)
import GHC.TypeLits
import qualified Numeric.LinearAlgebra.Static as LinAlg
import Numeric.LinearAlgebra.Static.Backprop
import Numeric.Backprop
import Lens.Micro.TH
import Control.DeepSeq (NFData)
import Data.Proxy
import Test.QuickCheck
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)

import Sized
import Data.GADT.Compare (GCompare)
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.Kind
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (DMap, (!))
import qualified Data.Dependent.Map as DMap

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

var2Getter :: SizedNetVar '(n, m) -> SizedNet d -> d '(n, m)
var2Getter SzWeights1 = szWeights1
var2Getter SzBias1    = szBias1
var2Getter SzWeights2 = szWeights2
var2Getter SzBias2    = szBias2
var2Getter SzWeights3 = szWeights3
var2Getter SzBias3    = szBias3
var2Getter SzWeights4 = szWeights4
var2Getter SzBias4    = szBias4

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

main :: IO ()
main = defaultMain [
           bgroup "eval" [
              env setupBackpropEnv    $ \tup ->
                  bench "backprop"    $ nf (uncurry (evalBP2 backpropTest)) tup
           ,  env setupSizedEnv       $ \tup ->
                  bench "ours"        $ nf (uncurry sizedTest) tup
           ,  env setupSizedEnv       $ \ ~(net, input) ->
                  bench "ours (dmap)" $ let dmap = mapFromNet net
                                         in nf (uncurry sizedDMapTest) (dmap, input)
           ]
       ,   bgroup "eval (dup)" [
              env setupBackpropEnv    $ \tup ->
                  bench "backprop"    $ nf (uncurry (evalBP2 backpropDupTest)) tup
           ,  env setupSizedEnv       $ \tup ->
                  bench "ours"        $ nf (uncurry sizedDupTest) tup
           ,  env setupSizedEnv       $ \ ~(net, input) ->
                  bench "ours (dmap)" $ let dmap = mapFromNet net
                                         in nf (uncurry sizedDMapDupTest) (dmap, input)
           ]
       ,   bgroup "derive" [
              env setupBackpropEnv   $ \ ~(net, input) ->
                  bench "backprop"   $ nf (gradBP (flip backpropTest (auto input))) net
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours"       $ nf (netFromMap . reverseAD (flip var2Getter net)) (exprSizedTest input)
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours +endo" $ nf (netFromMap . reverseADEndo (flip var2Getter net)) (exprSizedTest input)
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) -expr" $ let dmap = mapFromNet net
                                               in nf (netFromMap . reverseAD' (flip sizedDMapTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +endo -expr" $ let dmap = mapFromNet net
                                                     in nf (netFromMap . reverseAD'Endo (flip sizedDMapTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +topo -expr" $ let dmap = mapFromNet net
                                                     in nf (netFromMap . reverseADTopo (flip sizedDMapTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +endo +topo -expr" $ let dmap = mapFromNet net
                                                           in nf (netFromMap . reverseADTopoEndo (flip sizedDMapTest (fromMat input))) dmap
           ]
       ,   bgroup "derive (dup)" [
              env setupBackpropEnv   $ \ ~(net, input) ->
                  bench "backprop"   $ nf (gradBP (flip backpropDupTest (auto input))) net
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours"       $ nf (netFromMap . reverseAD (flip var2Getter net)) (exprSizedDupTest input)
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours +endo" $ nf (netFromMap . reverseADEndo (flip var2Getter net)) (exprSizedDupTest input)
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) -expr" $ let dmap = mapFromNet net
                                               in nf (netFromMap . reverseAD' (flip sizedDMapDupTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +endo -expr" $ let dmap = mapFromNet net
                                                     in nf (netFromMap . reverseAD'Endo (flip sizedDMapDupTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +topo -expr" $ let dmap = mapFromNet net
                                                     in nf (netFromMap . reverseADTopo (flip sizedDMapDupTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +endo +topo -expr" $ let dmap = mapFromNet net
                                                           in nf (netFromMap . reverseADTopoEndo (flip sizedDMapDupTest (fromMat input))) dmap
           ]
       ,   bgroup "derive (trip)" [
              env setupBackpropEnv   $ \ ~(net, input) ->
                  bench "backprop"   $ nf (gradBP (flip backpropTripTest (auto input))) net
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) -expr" $ let dmap = mapFromNet net
                                               in nf (netFromMap . reverseAD' (flip sizedDMapTripTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +endo -expr" $ let dmap = mapFromNet net
                                                     in nf (netFromMap . reverseAD'Endo (flip sizedDMapTripTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +topo -expr" $ let dmap = mapFromNet net
                                                     in nf (netFromMap . reverseADTopo (flip sizedDMapTripTest (fromMat input))) dmap
           ,  env setupSizedEnv      $ \ ~(net, input) ->
                  bench "ours (dmap) +endo +topo -expr" $ let dmap = mapFromNet net
                                                           in nf (netFromMap . reverseADTopoEndo (flip sizedDMapTripTest (fromMat input))) dmap
           ]
       ,   env setupSizedEnv $ \ ~(net, _) -> bench "netFromMap . mapFromNet" $ nf (netFromMap . mapFromNet) net
       ]
{-
main = defaultMain [
           env setupBackpropEnv $ \ ~tup@(net, input) -> bgroup "backprop library" [
              bench "eval"   $ nf (uncurry (evalBP2 backpropTest)) tup
           ,  bench "derive" $ nf (gradBP (flip backpropTest (auto input))) net
           ]
       ,   env setupBackpropEnv $ \ ~tup@(net, input) -> bgroup "backprop library (dup)" [
              bench "eval"   $ nf (uncurry (evalBP2 backpropDupTest)) tup
           ,  bench "derive" $ nf (gradBP (flip backpropDupTest (auto input))) net
           ]
       ,   env setupSizedEnv $ \ ~tup@(net, input) -> bgroup "our method" [
              bench "eval"   $ nf (uncurry sizedTest) tup
           ,  bench "derive" $ nf (netFromMap . reverseAD (flip var2Getter net)) (exprSizedTest input)
           ]
       ,   env setupSizedEnv $ \ ~tup@(net, input) -> bgroup "our method (dup)" [
              bench "eval"   $ nf (uncurry sizedDupTest) tup
           ,  bench "derive" $ nf (netFromMap . reverseAD (flip var2Getter net)) (exprSizedDupTest input)
           ]
       ]
-}
