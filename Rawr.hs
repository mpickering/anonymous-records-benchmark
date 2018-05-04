{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module Rawr where

-- Rawr
-- https://hackage.haskell.org/package/rawr

import Data.Rawr
import Data.Proxy
import Language.Haskell.TH
import THCommon

-- make a record of fieldBound+1 entries
--r :: R ( "x0" := Int, "x1" := Int, "x2" := Int, "x3" := Int )
mkDefs (\c ->
  [ mkRecord c (foldr
                (\x xs -> [| $x :*: $xs |])
                [| R0 |]
                [ [| R ( $(labelE ([c] ++ show n)) :=  $sn ) |]
                | n <- [ 0 .. fieldBound :: Int ],
                  let sn = [| n :: Int |] ]) ])
