{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, AllowAmbiguousTypes, OverloadedLabels #-}
module Rawr where

-- Rawr
-- https://hackage.haskell.org/package/rawr

import Data.Rawr
import Data.Proxy
import Language.Haskell.TH

--sequence [ valD (varP (mkName ("x"++l))) (normalB [| FldProxy :: $ty |]) []
--      | l <- map show [ 0 .. NN :: Int ],
--      let ty = [t| FldProxy $(litT (strTyLit l)) |] ]

--concrete :: R ( "x0" := Int, "x1" := Int )
--concrete = ( #x0 := (0 :: Int) ) :*: (#x1 := (0 :: Int) )


-- make a record of NN+1 entries
--r :: R ( "x0" := Int, "x1" := Int, "x2" := Int, "x3" := Int )
r =  $(
  foldr
    (\x xs -> [| $x :*: $xs |])
    [| R (#y := (0 :: Int)) |]
    [ [| R ( $(labelE ("x" ++ show n)) :=  $sn ) |]
        | n <- [ 0 .. NN :: Int ],
          let sn = [| n :: Int |] ])
