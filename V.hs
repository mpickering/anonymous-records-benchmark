{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, ScopedTypeVariables, FlexibleContexts #-}
module V where

import Data.Vinyl
import Data.Vinyl.Functor
import Language.Haskell.TH
import GHC.TypeLits
import Data.Proxy
import THCommon

r1 :: FieldRec ['("a", Integer), '("b", Char)]
r1 = Field @"a" 5 :& Field @"b" 'c' :& RNil

(=::) :: forall s t . KnownSymbol s => Proxy '(s, t) ->  t -> ElField '(s, t)
Proxy =:: t = Field @s t


fixRecord :: Rec ElField r -> Rec ElField r
fixRecord = id

-- define variables x0 .. xfieldBound to be labels
mkDefs (\c ->
  [valD (varP (mkName ([c]++l)))
                (normalB
                     (conE 'Proxy `sigE` [t| Proxy '($ty, Int) |]))
                []
      | l <- map show [ 0 .. fieldBound ],
      let ty = [t| $(litT (strTyLit l)) |] ]
  ++
   [ mkRecord c [| fixRecord $(
  foldr
    (\x xs -> [| $x <+> $xs |])
    [| RNil |]
    [ [| $(dyn ([c] ++ show n)) =: $sn |]
        | n <- [ 0 .. fieldBound :: Int ],
          let sn = [| n :: Int |] ]) |] ] )

type A = ['("0", Int), '("1", Int), '("2", Int)]

nestedUpdate :: Rec ElField A -> Rec ElField A
nestedUpdate v =
  rput (a2 =:: 10)  $
  rput (a2 =:: 10)  $
  rput (a2 =:: 10)  $
  rput (a2 =:: 10)  $
  rput (a2 =:: 10)  $
  v
