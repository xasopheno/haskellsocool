import Data.Ratio (numerator, denominator)

data PointOp = PointOp {
  point_fm :: Rational,
  point_fa :: Rational,
  point_lm :: Rational
}

instance Show PointOp where
  show op = "Fm " ++ showRational (point_fm op) ++ " | Fa " ++ showRational (point_fa op) ++ " | Lm " ++ showRational (point_lm op)
    where showRational r = show (numerator r) ++ "/" ++ show (denominator r)

data NormalForm = NormalForm [ SeqNormalForm ]
data SeqNormalForm = SeqNormalForm [ PointOp ]

instance Show NormalForm where
  show (NormalForm seqs) = "Overlay " ++ show seqs

instance Show SeqNormalForm where
  show (SeqNormalForm pointops) = "Seq " ++ show pointops

normalform = NormalForm [ 
   SeqNormalForm [ PointOp { point_fm = 1/2, point_fa = 1/3, point_lm = 1 } ]
  ]


-- ok now for the actual language

fm :: Rational -> NormalForm
fm r = NormalForm [ SeqNormalForm [ PointOp { point_fm = r , point_fa = 1, point_lm = 1 } ] ]

fa :: Rational -> NormalForm
fa r = NormalForm [ SeqNormalForm [ PointOp { point_fm = 1 , point_fa = r, point_lm = 1 } ] ]

lm :: Rational -> NormalForm
lm r = NormalForm [ SeqNormalForm [ PointOp { point_fm = 1 , point_fa = 1, point_lm = r } ] ]

-- helper
lengthRatio :: NormalForm -> Rational
lengthRatio (NormalForm []) = 0
lengthRatio (NormalForm (hd : tl)) = lrseq hd
  where lrseq (SeqNormalForm pointops) = foldr (+) 0 lrpointop pointops
        lrpointop pointop = point_lm pointop

-- assumes same number of voices
easyseq :: NormalForm -> NormalForm -> NormalForm
easyseq (NormalForm overlays1) (NormalForm overlays2) =
  NormalForm $ zipWith (++) overlays1 overlays2

-- assumes same length ratio
easyoverlay :: NormalForm -> NormalForm -> NormalForm
easyoverlay (NormalForm nf1) (NormalForm nf2) = NormalForm (nf1 ++ nf2)

-- easycompose :: NormalForm -> NormalForm -> NormalForm
-- easycompose 


main = putStrLn . show $ normalform
