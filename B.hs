{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module B where

-- Bookkeeper
-- https://hackage.haskell.org/package/bookkeeper


import Bookkeeper
import Language.Haskell.TH

-- make a record of NN+1 entries
r =  $(
  foldr
    (\x xs -> [| $xs & $x |])
    [| emptyBook |]
    [ [| ( $(labelE ("x" ++ show n)) =:  $sn ) |]
        | n <- [ 0 .. NN :: Int ],
          let sn = [| n :: Int |] ])
