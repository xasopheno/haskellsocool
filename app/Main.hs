module Main where

import Data.Ratio (denominator, numerator)

data PointOp = PointOp
  { fm :: Rational,
    lm :: Rational
    -- fa :: Rational,
    -- pm :: Rational,
    -- pa :: Rational,
    -- gm :: Rational
  }

data Op = Fm Rational | Lm Rational

class Normalize a where
  normalize :: a -> Op -> a

instance Normalize PointOp where
  normalize pointOp (Fm r) = applySimpleOp pointOp (Fm r)
  normalize pointOp (Lm r) = applySimpleOp pointOp (Lm r)

op1 =
  PointOp
    { fm = 1,
      lm = 1 / 2
    }

normalized = normalize op1 (Fm 3)

applySimpleOp :: PointOp -> Op -> PointOp
applySimpleOp pointOp (Fm r) = fmPointOp pointOp r
applySimpleOp pointOp (Lm r) = lmPointOp pointOp r

fmPointOp :: PointOp -> Rational -> PointOp
fmPointOp inputOp m =
  inputOp
    { fm = fm inputOp * m
    }

lmPointOp :: PointOp -> Rational -> PointOp
lmPointOp inputOp m =
  inputOp
    { lm = lm inputOp * m
    }

fmSeqNormalForm :: [PointOp] -> Rational -> [PointOp]
fmSeqNormalForm inputSeq m = map (\op -> applySimpleOp op (Fm m)) inputSeq

instance Show PointOp where
  show op = "Fm " ++ showRational (fm op) ++ " | Lm " ++ showRational (lm op)
    where
      showRational r = show (numerator r) ++ "/" ++ show (denominator r)

newtype NormalForm = NormalForm [SeqOp]

newtype SeqOp = SeqOp [PointOp]

instance Show NormalForm where
  show (NormalForm seqs) = "Overlay " ++ show seqs

instance Show SeqOp where
  show (SeqOp pointops) = "Seq " ++ show pointops

op = fmPointOp o 2
  where
    o =
      PointOp
        { fm = 1,
          -- fa = -3,
          lm = 1 / 2
        }

seqnormalform = [op]

result = fmSeqNormalForm seqnormalform 2

main :: IO ()
main = print normalized
