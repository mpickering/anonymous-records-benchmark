{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module E where

-- extensible
-- https://hackage.haskell.org/package/extensible


import Data.Extensible
import Language.Haskell.TH
import THCommon

-- Have to provide type signature
-- r :: Record '[ "x0" >: Int, "x1" >: Int, "x2" >: Int ]
mkDefs (\c ->
  [ (mkName [c]) `sigD` [t| Record $( foldr (\x xs -> promotedConsT `appT` x `appT` xs)
                       promotedNilT
                       [ [t| $(litT (strTyLit ([c] ++ show n))) >: Int |]
                       | n <- [ 0 .. NN :: Int ]] ) |] ]
  ++
  [ mkRecord c
    (foldr
      (\x xs -> [| $x <: $xs |])
      [| emptyRecord |]
      [ [| ( $(labelE ([c] ++ show n)) @=  $sn ) |]
          | n <- [ 0 .. NN :: Int ],
            let sn = [| n :: Int |] ]) ])
