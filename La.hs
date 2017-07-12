{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module La where

-- labels
-- https://hackage.haskell.org/package/labels

import Labels
import Language.Haskell.TH
import THCommon

-- make a record of NN+1 entries
mkDefs (\c ->
  [ mkRecord c (appsE $ (conE (tupleDataName ( NN + 1))) :
                [ [| $(labelE ([c] ++ show n)) := $sn |]
                  | n <- [ 0 .. NN :: Int ],
                  let sn = [| n :: Int |] ])])
