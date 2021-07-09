module Main where

import Data.Ratio (denominator, numerator)

data PointOp = PointOp
  { fm :: Rational,
    fa :: Rational,
    lm :: Rational,
    gm :: Rational,
    pm :: Rational,
    pa :: Rational
  }

newtype NormalForm = NormalForm [SeqOp]

newtype SeqOp = SeqOp [PointOp]

data Op
  = Fm Rational
  | Fa Rational
  | Lm Rational
  | Gm Rational
  | Pm Rational
  | Pa Rational

class Normalize a where
  new :: a
  normalize :: a -> Op -> a

fmOp :: PointOp -> Rational -> PointOp
fmOp inputOp m =
  inputOp
    { fm = fm inputOp * m
    }

faOp :: PointOp -> Rational -> PointOp
faOp op m =
  op
    { fa = fa op + m
    }

lmOp :: PointOp -> Rational -> PointOp
lmOp op m =
  op
    { lm = lm op * m
    }

gmOp :: PointOp -> Rational -> PointOp
gmOp op m =
  op
    { gm = gm op * m
    }

pmOp :: PointOp -> Rational -> PointOp
pmOp op m =
  op
    { pm = pm op * m
    }

paOp :: PointOp -> Rational -> PointOp
paOp op m =
  op
    { pa = pa op * m
    }

instance Show PointOp where
  show op = "Fm " ++ showRational (fm op) ++ " | Lm " ++ showRational (lm op)
    where
      showRational r = show (numerator r) ++ "/" ++ show (denominator r)

instance Show NormalForm where
  show (NormalForm seqs) = "Overlay " ++ show seqs

instance Show SeqOp where
  show (SeqOp pointops) = "Seq " ++ show pointops

instance Normalize PointOp where
  new =
    PointOp
      { fm = 1,
        fa = 0,
        lm = 1,
        gm = 1,
        pm = 1,
        pa = 0
      }
  normalize pointOp (Fm r) = fmOp pointOp r
  normalize pointOp (Fa r) = faOp pointOp r
  normalize pointOp (Lm r) = lmOp pointOp r
  normalize pointOp (Gm r) = gmOp pointOp r
  normalize pointOp (Pm r) = pmOp pointOp r
  normalize pointOp (Pa r) = paOp pointOp r

instance Normalize SeqOp where
  new = SeqOp [new :: PointOp]
  normalize (SeqOp ops) op = SeqOp result
    where
      result = map (`normalize` op) ops

instance Normalize NormalForm where
  new = NormalForm [new :: SeqOp]
  normalize (NormalForm seqOp) op = NormalForm result
    where
      result = map (`normalize` op) seqOp

normalform = new :: NormalForm

normalized = normalize normalform (Fm 2)

main :: IO ()
main = print normalized

--
--
--
--
--
--
--
--
--
--
--
--
--
