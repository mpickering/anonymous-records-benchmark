module THCommon where

import Language.Haskell.TH
import Data.Char

defBound = 4

-- n = 2 is fast
-- n = 3 is reasonable
-- n = 7 uses all my swap
-- n = 4 takes too long
fieldBound :: Int
fieldBound = 5

defNames = ['a' .. chr (ord 'a' + defBound) ]

mkDefs :: (Char -> [DecQ]) -> DecsQ
mkDefs gen = sequence $ concatMap gen defNames

mkRecord :: Char -> ExpQ -> DecQ
mkRecord c e =
  valD (varP (mkName [c])) (normalB e) []
