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

-- define variables x0 .. xNN to be labels
sequence [ valD (varP (mkName ("x"++l)))
                (normalB
                     (conE 'Proxy `sigE` [t| Proxy $ty |]))
                []
      | l <- map show [ 0 .. NN ],
      let ty = [t| $(litT (strTyLit l)) |] ]


instance Data.Record.Name (Proxy s) where
  name = Proxy

fixR :: f (Id KindStar) -> f (Id KindStar)
fixR = id

-- make a record of NN+1 entries
--r :: f (Id KindStar)
r = fixR $(
  foldr
    (\x xs -> [| $xs :& $x |])
    [| X |]
    [ [| $(dyn ("x" ++ show n)) := $sn |]
        | n <- [ 0 .. NN :: Int ],
          let sn = [| n :: Int |] ])
