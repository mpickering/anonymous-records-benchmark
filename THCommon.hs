module THCommon where

import Language.Haskell.TH

mkDefs :: (Char -> [DecQ]) -> DecsQ
mkDefs gen = sequence $ concatMap gen ['a' .. 'f']

mkRecord :: Char -> ExpQ -> DecQ
mkRecord c e =
  valD (varP (mkName [c])) (normalB e) []
