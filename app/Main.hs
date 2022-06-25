{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Monoid
import Data.Ratio (denominator, numerator)
import Data.Semigroup

data PointOp = PointOp
  { fm :: Rational,
    lm :: Rational
  }

newtype SeqOp
  = SeqOp
      [PointOp]
  deriving
    (Monoid, Semigroup)

data NormalForm = NormalForm
  { nf_ops :: [SeqOp],
    nf_length_ratio :: Rational
  }

instance Semigroup NormalForm where
  -- mappend
  NormalForm opsA lrA <> NormalForm opsB lrB =
    NormalForm (helper opsA opsB) lr
    where
      lr = lrA + lrB

      helper :: [SeqOp] -> [SeqOp] -> [SeqOp]
      [] `helper` seqOpsB = fmap (\(SeqOp ops) -> SeqOp (PointOp 0.0 lrA : ops)) seqOpsB
      seqOpsA `helper` [] = fmap (\(SeqOp ops) -> SeqOp (ops <> [PointOp 0.0 lrB])) seqOpsA
      (seqOpA : restA) `helper` (seqOpB : restB) = (seqOpA <> seqOpB) : (restA `helper` restB)

instance Monoid NormalForm where
  mempty =
    NormalForm
      { nf_ops = [],
        nf_length_ratio = 0.0
      }

data Op a
  = Fm Rational
  | Seq [a]

main :: IO ()
main = undefined
