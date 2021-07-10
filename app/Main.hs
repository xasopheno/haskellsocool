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

data Op a
  = Fm Rational
  | Fa Rational
  | Lm Rational
  | Gm Rational
  | Pm Rational
  | Pa Rational
  | Seq [a]

class Normalize a where
  new :: a
  normalize :: a -> Op a -> a

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

join a b = [a, b]

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

  normalize input (Fm r) = fmOp input r
  normalize input (Fa r) = faOp input r
  normalize input (Lm r) = lmOp input r
  normalize input (Gm r) = gmOp input r
  normalize input (Pm r) = pmOp input r
  normalize input (Pa r) = paOp input r
  normalize input (Seq []) = join input ops
  normalize input (Seq [ops]) = join input ops

instance Normalize SeqOp where
  new = SeqOp [new :: PointOp, new :: PointOp]
  normalize (SeqOp input) op = SeqOp result
    where
      result = map (`normalize` op) input

instance Normalize NormalForm where
  new = NormalForm [new :: SeqOp, new :: SeqOp]
  normalize (NormalForm input) op = NormalForm result
    where
      result = map (`normalize` op) input

normalform = new :: NormalForm

normalized = normalize normalform Seq (normalform)

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
