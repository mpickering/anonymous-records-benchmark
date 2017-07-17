{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, TypeApplications,
 DataKinds, PackageImports, PolyKinds #-}
module Re where

-- Records, like an old version of vinyl
-- https://hackage.haskell.org/package/records

import Data.Record
import qualified Data.Record.Combinators as C
import Language.Haskell.TH
import GHC.TypeLits
import Data.Proxy
import "kinds" Data.Kind
import Data.TypeFun
import THCommon

instance Data.Record.Name (Proxy s) where
  name = Proxy

fixR :: f (Id KindStar) -> f (Id KindStar)
fixR = id


-- define variables x0 .. xNN to be labels
mkDefs (\c -> [ valD (varP (mkName (c : l)))
                (normalB
                     (conE 'Proxy `sigE` [t| Proxy $ty |]))
                []
              | l <- map show [ 0 .. NN ],
              let ty = [t| $(litT (strTyLit l)) |] ]
              ++
   [ mkRecord c [| fixR $(
                  foldr
                    (\x xs -> [| $xs :& $x |])
                    [| X |]
                    [ [| $(dyn (c : show n)) := $sn |]
                    | n <- [ 0 .. NN :: Int ],
                    let sn = [| n :: Int |] ]) |]  ])

