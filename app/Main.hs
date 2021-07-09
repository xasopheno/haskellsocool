{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Ratio (denominator, numerator)

data PointOp = PointOp
  { fm :: Rational,
    fa :: Rational,
    lm :: Rational,
    gm :: Rational
    -- pm :: Rational,
    -- pa :: Rational,
  }

data Op = Fm Rational | Fa Rational | Lm Rational | Gm Rational

applySimpleOp :: PointOp -> Op -> PointOp
applySimpleOp op (Fm r) = fmOp op r
applySimpleOp op (Fa r) = faOp op r
applySimpleOp op (Lm r) = lmOp op r
applySimpleOp op (Gm r) = gmOp op r

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

class Normalize a where
  new :: a
  normalize :: a -> Op -> a

instance Normalize PointOp where
  new = PointOp {fm = 1, fa = 0, lm = 1, gm = 1}
  normalize pointOp (Fm r) = applySimpleOp pointOp (Fm r)
  normalize pointOp (Fa r) = applySimpleOp pointOp (Fa r)
  normalize pointOp (Lm r) = applySimpleOp pointOp (Lm r)
  normalize pointOp (Gm r) = applySimpleOp pointOp (Gm r)

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

op =
  PointOp
    { fm = 1,
      fa = 1,
      lm = 1,
      gm = 1
    }

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
