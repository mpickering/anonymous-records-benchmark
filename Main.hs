{-# OPTIONS_GHC -fcontext-stack=1000 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

#ifndef WITH_CTREX
#define WITH_CTREX (__GLASGOW_HASKELL__ > 707)
#endif

import Criterion.Main
import Criterion.Types
--import Criterion.Config
import Language.Haskell.TH

--import qualified H
--import qualified Hdup
import qualified V
import qualified R
import qualified L
import qualified La
import qualified DD
import qualified C
import qualified SR
import qualified SuperRecord as SR
import qualified Rawr as Rawr
import qualified B as B
import qualified E as E
import qualified Re as Re
import Data.OpenRecords -- https://github.com/atzeus/CTRex
        hiding (Rec)
import qualified Data.OpenRecords as Ctrex

import Control.DeepSeq
import Data.Monoid
import Data.Maybe

import Data.Vinyl
import Control.Lens
import Data.Proxy
import Control.Monad.Identity
import Data.Tagged
import qualified Data.Diverse.Many.Internal as DD
import qualified Bookkeeper as B
import qualified Labels as La
import qualified Data.Record.Combinators as Re
import qualified Data.Record as Re
import qualified Data.TypeFun as TF

import qualified GHC.Prim as P
import GHC.Generics

deriving instance Generic ((rec Re.:& field) style)
deriving instance Generic (Re.X style)
deriving instance Generic ((name Re.::: sort) style)

instance (NFData (rec style), NFData (field style)) => NFData ((rec Re.:& field) style) where
instance NFData (Re.X style) where
instance (NFData name, NFData (TF.App style sort)) => NFData ((name Re.::: sort) style) where



instance NFData t => NFData (ElField '(s, t)) where
  rnf (Field t) = rnf t

instance NFData (Rec f '[]) where
    rnf r = r `seq` ()

instance (NFData (Rec f as),
          NFData (f a)) => NFData (Rec f (a ': as)) where
    rnf (a :& as) = rnf a `seq` rnf as


instance (Ctrex.Forall r NFData) => NFData (Ctrex.Rec r) where
    rnf = rnf . Ctrex.erase (Proxy :: Proxy NFData) (\a -> rnf a `seq` ())

main = defaultMainWith
          (defaultConfig { csvFile = Just "Runtime.csv" }) $
          $(let
    maxOps = 5
    -- makes nf (\ end -> list `op` list `op` list `op` end) list
    -- when maxOps = 4
    mkGrp :: String -> Name -> Bool -> ExpQ -> ExpQ
    mkGrp title op assocR list =
      let fold | assocR = foldr | otherwise = foldl in
      [| bgroup $(stringE (title ++ ";" ++ show assocR))
          $(listE [ [| bench $(stringE (show n)) $
                        nf (\ end -> $(fold
                                (\a b -> [| $(varE op) $a $b |])
                                [| end |]
                                (replicate (n-1) list)
                          ))
                        $list |]
                | n <- [ 2 .. maxOps ] ]) |]


    mkLook :: String -> (Int -> ExpQ -> ExpQ) -> ExpQ -> ExpQ
    mkLook title lookup v =
        [| bgroup title
            [ bench "++" $
                nf
                    $(lam1E ((newName "v") >>= varP)
                         (foldr (\n b -> [| $(lookup n (dyn "v"))  + $b |])
                                    [| 0 |]
                                    [ 0 .. NN ] ))
                  $(v) ]
         |]

    vLookup n v = [| getField ($(v) ^. rlens $(dyn ("V.a"++show n))) |]
    lLookup n v = [| fromJust $ lookup $(dyn ("L.a"++ show n)) $(v) |]
    ddLookup :: Int -> ExpQ -> ExpQ
    ddLookup n v = [|  DD.fetchN $(sigE (conE 'Proxy) [t| Proxy $(litT (numTyLit (fromIntegral n))) |]) $(v) |]
    srLookup :: Int -> ExpQ -> ExpQ
    srLookup n v = [| SR.get $(dyn ("SR.a" ++ show n)) $(v) |]
    rLookup n v = [| $(dyn ("R.a"++show n)) $(v) |]

    cLookup n v = [| $(v) .! $(dyn ("C.a"++show n)) |]

    rawrLookup n v = [| $(labelE ("a" ++ show n)) $(v) |]

    bLookup n v = [| B.get $(labelE ("a" ++ show n)) $(v) |]

    eLookup n v = [| (view $(labelE ("a" ++ show n)) $(v)) :: Int |]

    laLookup n v = [| La.get $(labelE ("a" ++ show n)) $(v)|]

    reLookup n v = [| $(v) Re.!!! $(dyn ("Re.a" ++ show n))  |]

  in listE $ concat
        [ [
            mkGrp "C;append" '(.++) assocR [| C.a |],
            mkGrp "V;append" '(<+>) assocR [| V.a |],
            mkGrp "L;append"  '(++) assocR [| L.a |],
            mkGrp "DD;append"  '(DD././) assocR [| DD.a |],
            mkGrp "SR;append"  '(SR.++:) assocR [| SR.a |],
            -- Rawr checks for duplicates
            -- Bookkeeper also checks for duplicates
            -- Labels provides no machinery for append
            mkGrp "Re;append" 'Re.cat assocR [| Re.a |]
            ]  | assocR <- [False, True] ]
    ++ [

           mkLook "B;lookup" bLookup [| B.a |],
           mkLook "C;lookup" cLookup [| C.a |],
           mkLook "DD;lookup" ddLookup [| DD.a |],
           mkLook "E;lookup" eLookup [| E.a |],
           mkLook "LA;lookup" laLookup [| La.a |],
           mkLook "L;lookup" lLookup [| L.a |],
           mkLook "Rawr;lookup" rawrLookup [| Rawr.a |],
           mkLook "Re;lookup" reLookup [| Re.a |],
           mkLook "R;lookup" rLookup [| R.a |],
           mkLook "SR;lookup" srLookup [| SR.a |],
           mkLook "V;lookup" vLookup [| V.a |]]

  )

srLookup n v = [| SR.get $(dyn ("SR.x" ++ show n)) $(v) |]

myDefn = $((foldr (\n b -> [| (SR.get $(dyn ("SR.a" ++ show n)) SR.a)  + $b |])
                                     [| 0 |]
                                     [ 0 .. NN ] ))

