-- /home/rinat/autodiff/autodiff-exp/src/Sized.hs:227:1-15: Splicing declarations
instance Data.GADT.Internal.GEq Sized.Var where
  Data.GADT.Internal.geq Sized.X Sized.X
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq Sized.Y Sized.Y
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq _ _ = GHC.Maybe.Nothing
-- /home/rinat/autodiff/autodiff-exp/src/Sized.hs:228:1-20: Splicing declarations
instance Data.GADT.Internal.GCompare Sized.Var where
  Data.GADT.Internal.gcompare Sized.X Sized.X
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Sized.X {} _ = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Sized.X {} = Data.GADT.Internal.GGT
  Data.GADT.Internal.gcompare Sized.Y Sized.Y
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Sized.Y {} _ = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Sized.Y {} = Data.GADT.Internal.GGT
-- /home/rinat/autodiff/autodiff-exp/src/Sized.hs:229:1-17: Splicing declarations
instance Data.GADT.Internal.GShow Sized.Var where
  Data.GADT.Internal.gshowsPrec _ Sized.X = GHC.Show.showString "X"
  Data.GADT.Internal.gshowsPrec _ Sized.Y = GHC.Show.showString "Y"
-- /home/rinat/autodiff/autodiff-exp/src/Sized.hs:230:1-19: Splicing declarations
instance Data.Constraint.Extras.ArgDict c_a9dg Sized.Var where
  type Data.Constraint.Extras.ConstraintsFor Sized.Var c_a9dg = (c_a9dg '(4,
                                                                          3),
                                                                 c_a9dg '(3, 2))
  Data.Constraint.Extras.argDict
    = \case
        Sized.X {} -> Data.Constraint.Dict
        Sized.Y {} -> Data.Constraint.Dict
