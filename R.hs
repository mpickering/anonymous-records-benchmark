{-# LANGUAGE TemplateHaskell, CPP, DeriveAnyClass, DeriveGeneric #-}
module R where
import Language.Haskell.TH
import Data.Char
import THCommon
import Control.DeepSeq
import GHC.Generics (Generic)

mkDefs (\c ->
  let dcName = mkName [toUpper c]
  in
  [dataD (return []) dcName [] Nothing
    [recC dcName
      [  do
        ty <- [t| Int |]
        varBangType
                    (mkName ([c]++show n))
                    (bangType (bang (return NoSourceUnpackedness) (return NoSourceStrictness)) (return ty))
      | n <- [ 0 .. NN ] ] ] [derivClause Nothing [conT (mkName "Generic")
                                                  ,conT (mkName "NFData")]] ]
  ++
  [ mkRecord c (recConE dcName [  do
    e <- [| n |]
    return (mkName ([c] ++ show n), e)
        | n <- [0 .. NN :: Int ]] ) ] )
