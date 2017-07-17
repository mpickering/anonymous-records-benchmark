module THCommon where

import Language.Haskell.TH
import Data.Char

defBound = 4

defNames = ['a' .. chr (ord 'a' + defBound) ]

mkDefs :: (Char -> [DecQ]) -> DecsQ
mkDefs gen = sequence $ concatMap gen defNames

mkRecord :: Char -> ExpQ -> DecQ
mkRecord c e =
  valD (varP (mkName [c])) (normalB e) []
