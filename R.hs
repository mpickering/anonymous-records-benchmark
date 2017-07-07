{-# LANGUAGE TemplateHaskell, CPP #-}
module R where
import Language.Haskell.TH

fmap (:[]) $ dataD (return []) (mkName "X") [] Nothing
  [recC (mkName "X")
    [  do
      ty <- [t| Int |]
      varBangType
                  (mkName ("x"++show n))
                  (bangType (bang (return NoSourceUnpackedness) (return NoSourceStrictness)) (return ty))
     | n <- [ 0 .. NN ] ] ] (return [])

r :: X
r = $(recConE 'X [  do
    e <- [| n |]
    return (mkName ("x" ++ show n), e)
        | n <- [0 .. NN :: Int ]] )
