{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds #-}
module V where

import Data.Vinyl
import Language.Haskell.TH
import GHC.TypeLits
import Data.Proxy
import THCommon

r1 :: FieldRec ['("a", Integer), '("b", Char)]
r1 = Field @"a" 5 :& Field @"b" 'c' :& RNil

fixRecord :: Rec ElField b -> Rec ElField b
fixRecord x = x

-- define variables x0 .. xNN to be labels
mkDefs (\c ->
  [valD (varP (mkName ([c]++l)))
                (normalB
                     (conE 'Proxy `sigE` [t| Proxy '($ty, Int) |]))
                []
      | l <- map show [ 0 .. NN ],
      let ty = [t| $(litT (strTyLit l)) |] ]
  ++
   [ mkRecord c [| fixRecord $(
  foldr
    (\x xs -> [| $x <+> $xs |])
    [| RNil |]
    [ [| $(dyn ([c] ++ show n)) =: $sn |]
        | n <- [ 0 .. NN :: Int ],
          let sn = [| n :: Int |] ]) |] ] )
