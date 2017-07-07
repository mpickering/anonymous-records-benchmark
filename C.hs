{-# LANGUAGE CPP, TemplateHaskell, DataKinds #-}
module C where
import Data.OpenRecords -- https://github.com/atzeus/CTRex

import Language.Haskell.TH

-- define variables x0 .. xNN to be labels
sequence [ valD (varP (mkName ("x"++l))) (normalB [| Label :: $ty |]) []
      | l <- map show [ 0 .. NN :: Int ],
      let ty = [t| Label $(litT (strTyLit l)) |] ]


-- make a record of NN+1 entries
r = $(foldr
    (\x xs -> [| $x .| $xs |])
    [| empty |]
    [ [| $(dyn x) := (n :: Int) |]
        | n <- [ 0 .. NN :: Int ],
          let x = "x" ++show n ])
