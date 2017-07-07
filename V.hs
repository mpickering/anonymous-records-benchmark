{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds #-}
module V where

import Data.Vinyl
import Language.Haskell.TH
--import Data.Vinyl.Universe.Field
--import Data.Vinyl.TH
import GHC.TypeLits
import Data.Proxy

r1 :: FieldRec ['("a", Integer), '("b", Char)]
r1 = Field @"a" 5 :& Field @"b" 'c' :& RNil

-- define variables x0 .. xNN to be labels
sequence [ valD (varP (mkName ("x"++l)))
                (normalB
                     (conE 'Proxy `sigE` [t| Proxy '($ty, Int) |]))
                []
      | l <- map show [ 0 .. NN ],
      let ty = [t| $(litT (strTyLit l)) |] ]


fixRecord :: Rec ElField b -> Rec ElField b
fixRecord x = x

-- make a record of NN+1 entries
r = fixRecord $(
  foldr
    (\x xs -> [| $x <+> $xs |])
    [| RNil |]
    [ [| $(dyn ("x" ++ show n)) =: $sn |]
        | n <- [ 0 .. NN :: Int ],
          let sn = [| n :: Int |] ])
