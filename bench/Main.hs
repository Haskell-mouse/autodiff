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
import Env
import StagedTests
import Data.GADT.Compare (GCompare)
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.Kind
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (DMap, (!))
import qualified Data.Dependent.Map as DMap
import Sized.Staged (reverseADStaged, CSpliceQ (splice))
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax.Compat (liftCode, examineCode)

-- code = liftCode $ do
--   x <- runIO setupSizedNetStaged
--   examineCode (splice x)

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
              env setupBackpropEnv    $ \ ~(net, input) ->
                  bench "backprop"    $ nf (gradBP (flip backpropTest (auto input))) net
           ,  env setupSizedEnv       $ \ ~(net, input) ->
                  bench "ours"        $ nf (netFromMap . reverseAD (`var2Getter` net)) (exprSizedTest input)
       --     HERE BE DRAGONS
       --     ,  env setupSizedEnvStaged $ \ ~(net, input) ->
       --            bench "ours +th"    $ nf netFromMap ($$(reverseADStaged (`var2GetterStaged` net) (exprSizedTestStaged input)))
       --     ,  bench "ours +th"        $ nfIO $ (testAdIO reverseADStaged setupSizedNetStaged setupSizedInput)
       --     ,  bench "ours +th"        $ nfIO $$setupSizedEnvStaged
       --        `fmap` (\(net, input) -> netFromMap ($$(reverseADStaged (`var2GetterStaged` net)) (exprSizedTest $$input)))
       --     ,  bench "ours + th"       $ nfIO $ netFromMap <$> $$(testAdIO reverseADStaged setupSizedNet setupSizedInput)
           ,  env setupSizedEnv       $ \ ~(net, input) ->
                  bench "ours +endo"  $ nf (netFromMap . reverseADEndo (flip var2Getter net)) (exprSizedTest input)
           ,  env setupSizedEnv       $ \ ~(net, input) ->
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
