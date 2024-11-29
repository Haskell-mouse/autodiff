-- /home/rinat/autodiff/autodiff-exp/app/Env.hs:54:1-24: Splicing declarations
bpnBias1 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 200)
bpnBias1
  f_aiHq
  (Env.BackpropNet x1_aiHr x2_aiHs x3_aiHt x4_aiHu x5_aiHv x6_aiHw
                   x7_aiHx x8_aiHy)
  = (GHC.Base.fmap
       (\ y1_aiHz
          -> (((((((Env.BackpropNet x1_aiHr) y1_aiHz) x3_aiHt) x4_aiHu)
                  x5_aiHv)
                 x6_aiHw)
                x7_aiHx)
               x8_aiHy))
      (f_aiHq x2_aiHs)
{-# INLINE bpnBias1 #-}
bpnBias2 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 150)
bpnBias2
  f_aiHA
  (Env.BackpropNet x1_aiHB x2_aiHC x3_aiHD x4_aiHE x5_aiHF x6_aiHG
                   x7_aiHH x8_aiHI)
  = (GHC.Base.fmap
       (\ y1_aiHJ
          -> (((((((Env.BackpropNet x1_aiHB) x2_aiHC) x3_aiHD) y1_aiHJ)
                  x5_aiHF)
                 x6_aiHG)
                x7_aiHH)
               x8_aiHI))
      (f_aiHA x4_aiHE)
{-# INLINE bpnBias2 #-}
bpnBias3 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 100)
bpnBias3
  f_aiHK
  (Env.BackpropNet x1_aiHL x2_aiHM x3_aiHN x4_aiHO x5_aiHP x6_aiHQ
                   x7_aiHR x8_aiHS)
  = (GHC.Base.fmap
       (\ y1_aiHT
          -> (((((((Env.BackpropNet x1_aiHL) x2_aiHM) x3_aiHN) x4_aiHO)
                  x5_aiHP)
                 y1_aiHT)
                x7_aiHR)
               x8_aiHS))
      (f_aiHK x6_aiHQ)
{-# INLINE bpnBias3 #-}
bpnBias4 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 10)
bpnBias4
  f_aiHU
  (Env.BackpropNet x1_aiHV x2_aiHW x3_aiHX x4_aiHY x5_aiHZ x6_aiI0
                   x7_aiI1 x8_aiI2)
  = (GHC.Base.fmap
       (\ y1_aiI3
          -> (((((((Env.BackpropNet x1_aiHV) x2_aiHW) x3_aiHX) x4_aiHY)
                  x5_aiHZ)
                 x6_aiI0)
                x7_aiI1)
               y1_aiI3))
      (f_aiHU x8_aiI2)
{-# INLINE bpnBias4 #-}
bpnWeights1 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 200 784)
bpnWeights1
  f_aiI4
  (Env.BackpropNet x1_aiI5 x2_aiI6 x3_aiI7 x4_aiI8 x5_aiI9 x6_aiIa
                   x7_aiIb x8_aiIc)
  = (GHC.Base.fmap
       (\ y1_aiId
          -> (((((((Env.BackpropNet y1_aiId) x2_aiI6) x3_aiI7) x4_aiI8)
                  x5_aiI9)
                 x6_aiIa)
                x7_aiIb)
               x8_aiIc))
      (f_aiI4 x1_aiI5)
{-# INLINE bpnWeights1 #-}
bpnWeights2 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 150 200)
bpnWeights2
  f_aiIe
  (Env.BackpropNet x1_aiIf x2_aiIg x3_aiIh x4_aiIi x5_aiIj x6_aiIk
                   x7_aiIl x8_aiIm)
  = (GHC.Base.fmap
       (\ y1_aiIn
          -> (((((((Env.BackpropNet x1_aiIf) x2_aiIg) y1_aiIn) x4_aiIi)
                  x5_aiIj)
                 x6_aiIk)
                x7_aiIl)
               x8_aiIm))
      (f_aiIe x3_aiIh)
{-# INLINE bpnWeights2 #-}
bpnWeights3 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 100 150)
bpnWeights3
  f_aiIo
  (Env.BackpropNet x1_aiIp x2_aiIq x3_aiIr x4_aiIs x5_aiIt x6_aiIu
                   x7_aiIv x8_aiIw)
  = (GHC.Base.fmap
       (\ y1_aiIx
          -> (((((((Env.BackpropNet x1_aiIp) x2_aiIq) x3_aiIr) x4_aiIs)
                  y1_aiIx)
                 x6_aiIu)
                x7_aiIv)
               x8_aiIw))
      (f_aiIo x5_aiIt)
{-# INLINE bpnWeights3 #-}
bpnWeights4 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 10 100)
bpnWeights4
  f_aiIy
  (Env.BackpropNet x1_aiIz x2_aiIA x3_aiIB x4_aiIC x5_aiID x6_aiIE
                   x7_aiIF x8_aiIG)
  = (GHC.Base.fmap
       (\ y1_aiIH
          -> (((((((Env.BackpropNet x1_aiIz) x2_aiIA) x3_aiIB) x4_aiIC)
                  x5_aiID)
                 x6_aiIE)
                y1_aiIH)
               x8_aiIG))
      (f_aiIy x7_aiIF)
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
-- /home/rinat/autodiff/autodiff-exp/app/Env.hs:54:1-24: Splicing declarations
bpnBias1 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 200)
bpnBias1
  f_as4L
  (Env.BackpropNet x1_as4M x2_as4N x3_as4O x4_as4P x5_as4Q x6_as4R
                   x7_as4S x8_as4T)
  = (GHC.Base.fmap
       (\ y1_as4U
          -> (((((((Env.BackpropNet x1_as4M) y1_as4U) x3_as4O) x4_as4P)
                  x5_as4Q)
                 x6_as4R)
                x7_as4S)
               x8_as4T))
      (f_as4L x2_as4N)
{-# INLINE bpnBias1 #-}
bpnBias2 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 150)
bpnBias2
  f_as4V
  (Env.BackpropNet x1_as4W x2_as4X x3_as4Y x4_as4Z x5_as50 x6_as51
                   x7_as52 x8_as53)
  = (GHC.Base.fmap
       (\ y1_as54
          -> (((((((Env.BackpropNet x1_as4W) x2_as4X) x3_as4Y) y1_as54)
                  x5_as50)
                 x6_as51)
                x7_as52)
               x8_as53))
      (f_as4V x4_as4Z)
{-# INLINE bpnBias2 #-}
bpnBias3 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 100)
bpnBias3
  f_as55
  (Env.BackpropNet x1_as56 x2_as57 x3_as58 x4_as59 x5_as5a x6_as5b
                   x7_as5c x8_as5d)
  = (GHC.Base.fmap
       (\ y1_as5e
          -> (((((((Env.BackpropNet x1_as56) x2_as57) x3_as58) x4_as59)
                  x5_as5a)
                 y1_as5e)
                x7_as5c)
               x8_as5d))
      (f_as55 x6_as5b)
{-# INLINE bpnBias3 #-}
bpnBias4 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.R 10)
bpnBias4
  f_as5f
  (Env.BackpropNet x1_as5g x2_as5h x3_as5i x4_as5j x5_as5k x6_as5l
                   x7_as5m x8_as5n)
  = (GHC.Base.fmap
       (\ y1_as5o
          -> (((((((Env.BackpropNet x1_as5g) x2_as5h) x3_as5i) x4_as5j)
                  x5_as5k)
                 x6_as5l)
                x7_as5m)
               y1_as5o))
      (f_as5f x8_as5n)
{-# INLINE bpnBias4 #-}
bpnWeights1 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 200 784)
bpnWeights1
  f_as5p
  (Env.BackpropNet x1_as5q x2_as5r x3_as5s x4_as5t x5_as5u x6_as5v
                   x7_as5w x8_as5x)
  = (GHC.Base.fmap
       (\ y1_as5y
          -> (((((((Env.BackpropNet y1_as5y) x2_as5r) x3_as5s) x4_as5t)
                  x5_as5u)
                 x6_as5v)
                x7_as5w)
               x8_as5x))
      (f_as5p x1_as5q)
{-# INLINE bpnWeights1 #-}
bpnWeights2 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 150 200)
bpnWeights2
  f_as5z
  (Env.BackpropNet x1_as5A x2_as5B x3_as5C x4_as5D x5_as5E x6_as5F
                   x7_as5G x8_as5H)
  = (GHC.Base.fmap
       (\ y1_as5I
          -> (((((((Env.BackpropNet x1_as5A) x2_as5B) y1_as5I) x4_as5D)
                  x5_as5E)
                 x6_as5F)
                x7_as5G)
               x8_as5H))
      (f_as5z x3_as5C)
{-# INLINE bpnWeights2 #-}
bpnWeights3 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 100 150)
bpnWeights3
  f_as5J
  (Env.BackpropNet x1_as5K x2_as5L x3_as5M x4_as5N x5_as5O x6_as5P
                   x7_as5Q x8_as5R)
  = (GHC.Base.fmap
       (\ y1_as5S
          -> (((((((Env.BackpropNet x1_as5K) x2_as5L) x3_as5M) x4_as5N)
                  y1_as5S)
                 x6_as5P)
                x7_as5Q)
               x8_as5R))
      (f_as5J x5_as5O)
{-# INLINE bpnWeights3 #-}
bpnWeights4 ::
  Lens.Micro.Type.Lens' Env.BackpropNet (Internal.Static.L 10 100)
bpnWeights4
  f_as5T
  (Env.BackpropNet x1_as5U x2_as5V x3_as5W x4_as5X x5_as5Y x6_as5Z
                   x7_as60 x8_as61)
  = (GHC.Base.fmap
       (\ y1_as62
          -> (((((((Env.BackpropNet x1_as5U) x2_as5V) x3_as5W) x4_as5X)
                  x5_as5Y)
                 x6_as5Z)
                y1_as62)
               x8_as61))
      (f_as5T x7_as60)
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
