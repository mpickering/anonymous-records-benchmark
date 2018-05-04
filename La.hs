{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module La where

-- labels
-- https://hackage.haskell.org/package/labels

import Labels
import Language.Haskell.TH
import THCommon

-- make a record of fieldBound+1 entries
mkDefs (\c ->
  [ mkRecord c (appsE $ (conE (tupleDataName ( fieldBound + 1))) :
                [ [| $(labelE ([c] ++ show n)) := $sn |]
                  | n <- [ 0 .. fieldBound :: Int ],
                  let sn = [| n :: Int |] ])])
