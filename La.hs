{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module La where

-- labels
-- https://hackage.haskell.org/package/labels

import Labels
import Language.Haskell.TH

-- make a record of NN+1 entries
r =  $( appsE $ (conE (tupleDataName ( NN + 1))) :
                [ [| $(labelE ("x" ++ show n)) := $sn |]
                  | n <- [ 0 .. NN :: Int ],
                  let sn = [| n :: Int |] ])
