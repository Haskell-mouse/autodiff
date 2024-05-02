-- /home/rinat/autodiff/autodiff-exp/app/Env.hs:54:1-24: Splicing declarations
bpnBias1 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 200)
bpnBias1
  f_ajgR
  (Env.BackpropNet x1_ajgS x2_ajgT x3_ajgU x4_ajgV x5_ajgW x6_ajgX
                   x7_ajgY x8_ajgZ)
  = (GHC.Base.fmap
       (\ y1_ajh0
          -> (((((((Env.BackpropNet x1_ajgS) y1_ajh0) x3_ajgU) x4_ajgV)
                  x5_ajgW)
                 x6_ajgX)
                x7_ajgY)
               x8_ajgZ))
      (f_ajgR x2_ajgT)
{-# INLINE bpnBias1 #-}
bpnBias2 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 150)
bpnBias2
  f_ajh1
  (Env.BackpropNet x1_ajh2 x2_ajh3 x3_ajh4 x4_ajh5 x5_ajh6 x6_ajh7
                   x7_ajh8 x8_ajh9)
  = (GHC.Base.fmap
       (\ y1_ajha
          -> (((((((Env.BackpropNet x1_ajh2) x2_ajh3) x3_ajh4) y1_ajha)
                  x5_ajh6)
                 x6_ajh7)
                x7_ajh8)
               x8_ajh9))
      (f_ajh1 x4_ajh5)
{-# INLINE bpnBias2 #-}
bpnBias3 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 100)
bpnBias3
  f_ajhb
  (Env.BackpropNet x1_ajhc x2_ajhd x3_ajhe x4_ajhf x5_ajhg x6_ajhh
                   x7_ajhi x8_ajhj)
  = (GHC.Base.fmap
       (\ y1_ajhk
          -> (((((((Env.BackpropNet x1_ajhc) x2_ajhd) x3_ajhe) x4_ajhf)
                  x5_ajhg)
                 y1_ajhk)
                x7_ajhi)
               x8_ajhj))
      (f_ajhb x6_ajhh)
{-# INLINE bpnBias3 #-}
bpnBias4 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 10)
bpnBias4
  f_ajhl
  (Env.BackpropNet x1_ajhm x2_ajhn x3_ajho x4_ajhp x5_ajhq x6_ajhr
                   x7_ajhs x8_ajht)
  = (GHC.Base.fmap
       (\ y1_ajhu
          -> (((((((Env.BackpropNet x1_ajhm) x2_ajhn) x3_ajho) x4_ajhp)
                  x5_ajhq)
                 x6_ajhr)
                x7_ajhs)
               y1_ajhu))
      (f_ajhl x8_ajht)
{-# INLINE bpnBias4 #-}
bpnWeights1 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 200 784)
bpnWeights1
  f_ajhv
  (Env.BackpropNet x1_ajhw x2_ajhx x3_ajhy x4_ajhz x5_ajhA x6_ajhB
                   x7_ajhC x8_ajhD)
  = (GHC.Base.fmap
       (\ y1_ajhE
          -> (((((((Env.BackpropNet y1_ajhE) x2_ajhx) x3_ajhy) x4_ajhz)
                  x5_ajhA)
                 x6_ajhB)
                x7_ajhC)
               x8_ajhD))
      (f_ajhv x1_ajhw)
{-# INLINE bpnWeights1 #-}
bpnWeights2 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 150 200)
bpnWeights2
  f_ajhF
  (Env.BackpropNet x1_ajhG x2_ajhH x3_ajhI x4_ajhJ x5_ajhK x6_ajhL
                   x7_ajhM x8_ajhN)
  = (GHC.Base.fmap
       (\ y1_ajhO
          -> (((((((Env.BackpropNet x1_ajhG) x2_ajhH) y1_ajhO) x4_ajhJ)
                  x5_ajhK)
                 x6_ajhL)
                x7_ajhM)
               x8_ajhN))
      (f_ajhF x3_ajhI)
{-# INLINE bpnWeights2 #-}
bpnWeights3 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 100 150)
bpnWeights3
  f_ajhP
  (Env.BackpropNet x1_ajhQ x2_ajhR x3_ajhS x4_ajhT x5_ajhU x6_ajhV
                   x7_ajhW x8_ajhX)
  = (GHC.Base.fmap
       (\ y1_ajhY
          -> (((((((Env.BackpropNet x1_ajhQ) x2_ajhR) x3_ajhS) x4_ajhT)
                  y1_ajhY)
                 x6_ajhV)
                x7_ajhW)
               x8_ajhX))
      (f_ajhP x5_ajhU)
{-# INLINE bpnWeights3 #-}
bpnWeights4 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 10 100)
bpnWeights4
  f_ajhZ
  (Env.BackpropNet x1_aji0 x2_aji1 x3_aji2 x4_aji3 x5_aji4 x6_aji5
                   x7_aji6 x8_aji7)
  = (GHC.Base.fmap
       (\ y1_aji8
          -> (((((((Env.BackpropNet x1_aji0) x2_aji1) x3_aji2) x4_aji3)
                  x5_aji4)
                 x6_aji5)
                y1_aji8)
               x8_aji7))
      (f_ajhZ x7_aji6)
{-# INLINE bpnWeights4 #-}
-- /home/rinat/autodiff/autodiff-exp/app/Env.hs:119:1-23: Splicing declarations
instance Data.GADT.Internal.GEq Env.SizedNetVar where
  Data.GADT.Internal.geq Env.SzWeights1 Env.SzWeights1
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq Env.SzBias1 Env.SzBias1
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq Env.SzWeights2 Env.SzWeights2
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq Env.SzBias2 Env.SzBias2
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq Env.SzWeights3 Env.SzWeights3
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq Env.SzBias3 Env.SzBias3
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq Env.SzWeights4 Env.SzWeights4
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq Env.SzBias4 Env.SzBias4
    = do GHC.Base.return Data.Type.Equality.Refl
  Data.GADT.Internal.geq _ _ = GHC.Maybe.Nothing
-- /home/rinat/autodiff/autodiff-exp/app/Env.hs:120:1-28: Splicing declarations
instance Data.GADT.Internal.GCompare Env.SizedNetVar where
  Data.GADT.Internal.gcompare Env.SzWeights1 Env.SzWeights1
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Env.SzWeights1 {} _
    = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Env.SzWeights1 {}
    = Data.GADT.Internal.GGT
  Data.GADT.Internal.gcompare Env.SzBias1 Env.SzBias1
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Env.SzBias1 {} _
    = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Env.SzBias1 {}
    = Data.GADT.Internal.GGT
  Data.GADT.Internal.gcompare Env.SzWeights2 Env.SzWeights2
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Env.SzWeights2 {} _
    = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Env.SzWeights2 {}
    = Data.GADT.Internal.GGT
  Data.GADT.Internal.gcompare Env.SzBias2 Env.SzBias2
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Env.SzBias2 {} _
    = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Env.SzBias2 {}
    = Data.GADT.Internal.GGT
  Data.GADT.Internal.gcompare Env.SzWeights3 Env.SzWeights3
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Env.SzWeights3 {} _
    = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Env.SzWeights3 {}
    = Data.GADT.Internal.GGT
  Data.GADT.Internal.gcompare Env.SzBias3 Env.SzBias3
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Env.SzBias3 {} _
    = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Env.SzBias3 {}
    = Data.GADT.Internal.GGT
  Data.GADT.Internal.gcompare Env.SzWeights4 Env.SzWeights4
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Env.SzWeights4 {} _
    = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Env.SzWeights4 {}
    = Data.GADT.Internal.GGT
  Data.GADT.Internal.gcompare Env.SzBias4 Env.SzBias4
    = Data.GADT.Compare.Monad.runGComparing
        (do GHC.Base.return Data.GADT.Internal.GEQ)
  Data.GADT.Internal.gcompare Env.SzBias4 {} _
    = Data.GADT.Internal.GLT
  Data.GADT.Internal.gcompare _ Env.SzBias4 {}
    = Data.GADT.Internal.GGT
