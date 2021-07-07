module Main where

import Data.Ratio (denominator, numerator)

data PointOp = PointOp
  { fm :: Rational,
    fa :: Rational,
    lm :: Rational
    -- pm :: Rational,
    -- pa :: Rational,
    -- gm :: Rational
  }

fmPointOp :: PointOp -> Rational -> PointOp
fmPointOp inputOp m =
  inputOp
    { fm = fm inputOp * m
    }

instance Show PointOp where
  show op = "Fm " ++ showRational (fm op) ++ " | Fa " ++ showRational (fa op) ++ " | Lm " ++ showRational (lm op)
    where
      showRational r = show (numerator r) ++ "/" ++ show (denominator r)

newtype NormalForm = NormalForm [SeqNormalForm]

newtype SeqNormalForm = SeqNormalForm [PointOp]

instance Show NormalForm where
  show (NormalForm seqs) = "Overlay " ++ show seqs

instance Show SeqNormalForm where
  show (SeqNormalForm pointops) = "Seq " ++ show pointops

op = fmPointOp o 10
  where
    o = PointOp {fm = 1 / 2, fa = -3, lm = 1}

normalform =
  NormalForm
    [ SeqNormalForm
        [ op
        ]
    ]

main :: IO ()
main = print normalform
