module Main where

import Data.List (intercalate)
import Data.Ratio (denominator, numerator)

data PointOp = PointOp
  { fm :: Rational,
    fa :: Rational,
    lm :: Rational,
    gm :: Rational,
    pm :: Rational,
    pa :: Rational
  }
  deriving (Show)

newtype NormalForm = NormalForm [SeqOp]
  deriving (Show)

newtype SeqOp = SeqOp [PointOp]
  deriving (Show)

data Op
  = Fm Rational
  | Fa Rational
  | Lm Rational
  | Gm Rational
  | Pm Rational
  | Pa Rational

class Display a where
  toString :: a -> String

instance Display PointOp where
  toString op = "Fm " ++ showRational (fm op) ++ ", Lm " ++ showRational (lm op)
    where
      showRational r = show (numerator r) ++ "/" ++ show (denominator r)

instance Display NormalForm where
  toString (NormalForm seqs) = "Overlay " ++ toString seqs

instance Display SeqOp where
  toString (SeqOp pointops) = "Seq " ++ toString pointops

instance Display a => Display [a] where
  toString list = "[" ++ intercalate ", " (map toString list) ++ "]"

normalizePointOp :: Op -> PointOp -> PointOp
normalizePointOp op pointOp = case op of
  Fm r ->
    pointOp
      { fm = fm pointOp * r
      }
  Fa r ->
    pointOp
      { fa = fa pointOp * r
      }
  Gm r ->
    pointOp
      { gm = gm pointOp * r
      }
  Lm r ->
    pointOp
      { lm = lm pointOp * r
      }
  Pm r ->
    pointOp
      { pm = pm pointOp * r
      }
  Pa r ->
    pointOp
      { pa = pa pointOp * r
      }

normalize :: NormalForm -> Op -> NormalForm
normalize (NormalForm normalform) op =
  NormalForm
    ( map (\(SeqOp x) -> SeqOp (map (normalizePointOp op) x)) normalform
    )

normalform =
  NormalForm
    [ SeqOp
        [ PointOp
            { fm = 1,
              fa = 0,
              lm = 1,
              gm = 1,
              pm = 1,
              pa = 0
            }
        ]
    ]

normalized = normalize normalform (Fm 2)

main :: IO ()
main = do
  putStrLn $ toString normalized
  print normalized
