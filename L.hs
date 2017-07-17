{-# LANGUAGE CPP, TemplateHaskell #-}
module L where

import Language.Haskell.TH
import THCommon

updateList :: String -> a -> [(String, a)] -> [(String, a)]
updateList t x [] = []
updateList t x ((k , v) : kvs)
      | t == k = ((k, x) : kvs)
      | otherwise = (k, v) : updateList t x kvs

mkDefs (\c ->

    [ valD (varP (mkName ([c] ++ n)))
                  (normalB [| n |]) []
              | n <- map show [0 .. NN :: Int]]

    ++ [ (mkName [c]) `sigD` [t| [(String, Int)] |]]
    ++ [ mkRecord c
                (listE [ [| ( $(varE (mkName ([c] ++ show n))),  n :: Int ) |]
                | n <- [0 .. NN :: Int] ] ) ])
