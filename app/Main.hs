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

fmPointOp :: PointOp -> Rational -> PointOp
fmPointOp inputOp m =
  inputOp
    { 
      fm = fm inputOp * m
    }

fmSeqNormalForm :: SeqOp -> Rational -> SeqOp
fmSeqNormalForm inputSeq m = map helper inputSeq
  where helper = \op -> fmPointOp op m


instance Show PointOp where
  show op = "Fm " ++ showRational (fm op) ++ " | Lm " ++ showRational (lm op)
    where
      showRational r = show (numerator r) ++ "/" ++ show (denominator r)

data NormalForm = NormalForm [SeqOp]

data SeqOp = SeqOp [PointOp]

instance Show NormalForm where
  show (NormalForm seqs) = "Overlay " ++ show seqs

instance Show SeqOp where
  show (SeqOp pointops) = "Seq " ++ show pointops

op = fmPointOp o 2
  where
    o = PointOp {
      fm = 1,
      -- fa = -3, 
      lm = 1/2
    }

seqnormalform =  SeqOp [ op ]

result = fmSeqNormalForm seqnormalform 2

main :: IO ()
main = print result
