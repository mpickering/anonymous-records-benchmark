{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds #-}
module SR where


import SuperRecord
import Data.Proxy
import Language.Haskell.TH
import THCommon


mkDefs (\c ->

  [ valD (varP (mkName ([c]++l))) (normalB [| FldProxy :: $ty |]) []
      | l <- map show [ 0 .. fieldBound :: Int ],
      let ty = [t| FldProxy $(litT (strTyLit l)) |] ]
  ++
  [ mkRecord c (foldr
                (\x xs -> [| $x `rcons` $xs |])
                [| rnil |]
                [ [| $(dyn([c] ++ show n)) :=  $sn |]
                | n <- [ 0 .. fieldBound :: Int ],
                  let sn = [| n :: Int |] ])] )
