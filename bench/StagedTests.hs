module StagedTests (module StagedTests) where

import Data.Dependent.Map (DMap)
import GHC.TypeLits (KnownNat)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax.Compat (Code (examineCode), SpliceQ, liftCode)
import Sized
import Sized.Staged
import Sized.LiftInstances ()
import Env

-- This file is still being built.
-- While I haven't got to the last version of a testing function,
-- different iterations of it will be kept in comments.

-- testAdIO ::
--     (SizedSemiring d, forall tup. Semigroup (d tup)) =>
--     AutoDiffStagedType -> IO (CSpliceQ SizedNet d) -> IO (Mat '(784, 1)) -> SpliceQ (DMap SizedNetVar d)
-- testAdIO adFunc netIO inputIO = liftCode $ do
--     net <- runIO netIO
--     input <- runIO inputIO
--     examineCode [|| $$(adFunc (`var2GetterStaged` net) (exprSizedTestStaged input)) ||]

-- testAdIO ::
--     (SizedSemiring d, forall tup. Semigroup (d tup)) =>
--     AutoDiffStagedType -> IO (CSpliceQ SizedNet d) -> IO (Mat '(784, 1)) -> SpliceQ (IO (DMap SizedNetVar d))
-- testAdIO adFunc netIO inputIO = [||
--     inputIO >>= (\input -> return $ liftCode $ do
--         net <- runIO netIO
--         examineCode $$(adFunc (`var2GetterStaged` net) (exprSizedTestStaged input))) ||]

-- testAdIO ::
--   ( SizedSemiring d,
--     forall tup. Semigroup (d tup),
--     (forall n m. (KnownNat n, KnownNat m) => Lift (d '(n, m)))
--   ) =>
--   AutoDiffStagedType ->
--   IO (SizedNet d) ->
--   IO (Mat '(784, 1)) ->
--   SpliceQ (DMap SizedNetVar d)
-- testAdIO adFunc netIO inputIO = liftCode $ do
--   net <- runIO netIO
--   input <- runIO inputIO
--   examineCode $ adFunc (`var2GetterStaged` net) (exprSizedTestStaged input)

-- This is the current version of the testing function.
-- Although it typechecks, its expansion takes too long to compile.
testAdIO ::
  ( SizedSemiring d,
    forall tup. Semigroup (d tup),
    (forall n m. (KnownNat n, KnownNat m) => Lift (d '(n, m)))
  ) =>
  AutoDiffStagedType ->
  IO (SizedNet d) ->
  IO (Mat '(784, 1)) ->
  SpliceQ (IO (DMap SizedNetVar d))
testAdIO adFunc netIO inputIO = liftCode $ do
  input <- runIO inputIO
  examineCode [|| do
    net <- netIO
    return $$(adFunc (\var -> CSpliceQ [|| ($$(splice var) `var2Getter` net) ||]) (exprSizedTestStaged input)) ||]

-- testAdIO ::
--   ( SizedSemiring d,
--     forall tup. Semigroup (d tup),
--     (forall n m. (KnownNat n, KnownNat m) => Lift (d '(n, m)))
--   ) =>
--   AutoDiffStagedType ->
--   IO (SizedNet d) ->
--   IO (Mat '(784, 1)) ->
--   SpliceQ (IO (DMap SizedNetVar d))
-- testAdIO adFunc netIO inputIO = [|| do
--     net <- netIO
--     input <- inputIO
--     return
--         $$(adFunc
--             (\x -> CSpliceQ [|| ($$(splice x) `var2Getter` net) ||])
--             $$(splice (exprSizedTestStaged (CSpliceQ [|| input ||])))) ||]
