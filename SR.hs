{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds #-}
module SR where


import SuperRecord
import Data.Proxy
import Language.Haskell.TH


--fixRecord :: Rec ElField b -> Rec ElField b
--fixRecord x = x
--
sequence [ valD (varP (mkName ("x"++l))) (normalB [| FldProxy :: $ty |]) []
      | l <- map show [ 0 .. NN :: Int ],
      let ty = [t| FldProxy $(litT (strTyLit l)) |] ]

-- make a record of NN+1 entries
r =  $(
  foldr
    (\x xs -> [| $x `rcons` $xs |])
    [| rnil |]
    [ [| $(dyn("x" ++ show n)) :=  $sn |]
        | n <- [ 0 .. NN :: Int ],
          let sn = [| n :: Int |] ])
