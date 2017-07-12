{-# LANGUAGE CPP, TemplateHaskell, DataKinds #-}
module C where
import Data.OpenRecords -- https://github.com/atzeus/CTRex

import Language.Haskell.TH
import THCommon

-- define variables x0 .. xNN to be labels
mkDefs (\c ->
  [valD (varP (mkName ([c]++l))) (normalB [| Label :: $ty |]) []
      | l <- map show [ 0 .. NN :: Int ],
      let ty = [t| Label $(litT (strTyLit l)) |] ]
  ++ [mkRecord c
      (foldr
        (\x xs -> [| $x .| $xs |])
        [| empty |]
        [ [| $(dyn x) := (n :: Int) |]
           | n <- [ 0 .. NN :: Int ],
             let x = [c] ++show n ])])
