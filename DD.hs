{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds #-}
module DD where


import Data.Diverse
import Data.Proxy
import Language.Haskell.TH


--fixRecord :: Rec ElField b -> Rec ElField b
--fixRecord x = x

-- make a record of NN+1 entries
r =  $(
  foldr
    (\x xs -> [| $x /./ $xs |])
    [| nil |]
    [ [| single $sn |]
        | n <- [ 0 .. NN :: Int ],
          let sn = [| n :: Int |] ])
