{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds #-}
module DD where


import Data.Diverse
import Data.Proxy
import Language.Haskell.TH
import THCommon


-- make a record of NN+1 entries
mkDefs (\c ->
  [ mkRecord c (foldr
              (\x xs -> [| $x /./ $xs |])
              [| nil |]
              [ [| single $sn |]
              | n <- [ 0 .. NN :: Int ],
                let sn = [| n :: Int |] ]) ] )
