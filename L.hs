{-# LANGUAGE CPP, TemplateHaskell #-}
module L where

import Language.Haskell.TH

sequence [ valD (varP (mkName ("x" ++ n)))
                (normalB [| n |]) []
            | n <- map show [0 .. NN :: Int]]


r :: [(String, Int)]
r = $(listE [ [| ( $(varE (mkName ("x" ++ show n))),  n :: Int ) |]
              | n <- [0 .. NN :: Int] ] )
