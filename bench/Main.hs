module Main (main) where

import Criterion.Main

import Numeric.Backprop

import Env
import Sized
import Sized.Staged
import StagedTests

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
              env setupBackpropEnv         $ \ ~(net, input) ->
                  bench "backprop"         $ nf (gradBP (flip backpropTest (auto input))) net
           ,  env setupSizedEnv            $ \ ~(net, input) ->
                  bench "ours"             $ nf (netFromMap . reverseAD (`var2Getter` net)) (exprSizedTest input)
--           , env setupSizedNet             $ \ net ->
--                  bench "ours + th"        $ nf (netFromMap . $$(testAdStaged reverseADStaged)) net
           ,  env setupSizedEnv            $ \ ~(net, input) ->
                  bench "ours +endo"       $ nf (netFromMap . reverseADEndo (flip var2Getter net)) (exprSizedTest input)
           , env setupSizedNet             $ \ net ->
                  bench "ours + endo + th" $ nf (netFromMap . $$(testAdStaged reverseADEndoStaged)) net
           ,  env setupSizedEnv             $ \ ~(net, input) ->
                  bench "ours (dmap) -expr" $ let dmap = mapFromNet net
                                               in nf (netFromMap . reverseAD' (flip sizedDMapTest (fromMat input))) dmap
           ,  env setupSizedEnv             $ \ ~(net, input) ->
                  bench "ours (dmap) +endo -expr" $ let dmap = mapFromNet net
                                                     in nf (netFromMap . reverseAD'Endo (flip sizedDMapTest (fromMat input))) dmap
           ,  env setupSizedEnv             $ \ ~(net, input) ->
                  bench "ours (dmap) +topo -expr" $ let dmap = mapFromNet net
                                                     in nf (netFromMap . reverseADTopo (flip sizedDMapTest (fromMat input))) dmap
           ,  env setupSizedEnv             $ \ ~(net, input) ->
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
