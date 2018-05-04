{-OPTIONS_GHC -fcontext-stack=2000 #-}
{-# LANGUAGE CPP, FlexibleContexts, TemplateHaskell #-}
module H where

import Data.HList.CommonMain
import GHC.TypeLits -- needed for ghc-7.6
import Language.Haskell.TH

-- no duplicate check
x .**. Record r = Record (HCons x r)

makeLabelable (unwords [ "x" ++ show n | n <- [0 .. fieldBound]])

r = $(foldr
        (\x xs ->  [| $x .**. $xs |])
        [| emptyRecord |]
        [ [| $(dyn ("x"++ show l)) .==. (l :: Int) |]
          | l <- [0 .. fieldBound :: Int] ]

