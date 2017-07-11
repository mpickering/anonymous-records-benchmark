{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module E where

-- extensible
-- https://hackage.haskell.org/package/extensible


import Data.Extensible
import Language.Haskell.TH

-- make a record of NN+1 entries
-- Have to provide type signature
-- r :: Record '[ "x0" >: Int, "x1" >: Int, "x2" >: Int ]
r :: Record $( foldr (\x xs -> promotedConsT `appT` x `appT` xs)
                     promotedNilT
                     [ [t| $(litT (strTyLit ("x" ++ show n))) >: Int |] | n <- [ 0 .. NN :: Int ]] )
r =  $(
  foldr
    (\x xs -> [| $x <: $xs |])
    [| emptyRecord |]
    [ [| ( $(labelE ("x" ++ show n)) @=  $sn ) |]
        | n <- [ 0 .. NN :: Int ],
          let sn = [| n :: Int |] ])
