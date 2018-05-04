{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module B where

-- Bookkeeper
-- https://hackage.haskell.org/package/bookkeeper


import Bookkeeper
import Language.Haskell.TH
import THCommon

-- make a record of fieldBound+1 entries
mkDefs (\c -> [
  mkRecord c
    (
    foldr
      (\x xs -> [| $xs & $x |])
      [| emptyBook |]
      [ [| ( $(labelE ([c] ++ show n)) =:  $sn ) |]
          | n <- [ 0 .. fieldBound :: Int ],
            let sn = [| n :: Int |] ]) ])
