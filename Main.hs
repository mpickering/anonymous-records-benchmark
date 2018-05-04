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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS -fno-full-laziness #-}
module Main where

#ifndef WITH_CTREX
#define WITH_CTREX (__GLASGOW_HASKELL__ > 707)
#endif

import Criterion.Main
import Criterion.Types
--import Criterion.Config
import Language.Haskell.TH
import THCommon

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
import qualified Bookkeeper.Internal as B
import qualified Labels as La
import qualified Data.Record.Combinators as Re
import qualified Data.Record as Re
import qualified Data.TypeFun as TF
import qualified Data.Vinyl as V
import qualified Data.Rawr as Rawr
import qualified Data.OpenRecords as C
import qualified Data.Type.Map as B


import qualified GHC.Prim as P
import GHC.Generics

instance NFData v => NFData ((La.:=) s v) where
  rnf (p La.:= v) = rnf v



instance NFData (B.Book' r) where
instance NFData (B.Map r) where
  rnf (B.Empty) = ()
  rnf (B.Ext k v m) = rnf m

deriving instance Generic (B.Book' r)

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
    mkGrp :: String -> Name -> ExpQ
    mkGrp title op =
      [| bgroup title |] `appE`
      (listE [
        let fold | assocR = foldr | otherwise = foldl
            list = varE (mkName (title ++ ".a")) in
        [| bgroup $(stringE (show assocR))
          $(listE [ [| bench $(stringE (show n)) $
                        nf (\ end -> $(fold
                                (\a b -> [| $(varE op) $a $b |])
                                [| end |]
                                (replicate (n-1) list)
                          ))
                        $list |]
                  | n <- [ 2 .. maxOps ] ]) |] | assocR <- [True, False]])

    -- Makes an append example but of distinct records
    mkAppendDiff :: String -> Name -> ExpQ ->  ExpQ
    mkAppendDiff title op list =
      [| bench $(stringE title) $
          nf $(lam1E lamPat (foldr (\a b -> appsE [varE op, a, b])
                             list
                             [ varE (mkName ("x" ++ show n)) | n <- [0 .. defBound ]])) $(genTup title) |]
      where
        lamPat = tupP [ varP (mkName ("x" ++ show n)) | n <- [0 .. defBound] ]


    mkLook :: String -> (Int -> ExpQ -> ExpQ) -> ExpQ
    mkLook title lookup =
        [| bench title $
                nf
                    $(lam1E ((newName "v") >>= varP)
                         (foldr (\n b -> [| $(lookup n (dyn "v"))  + $b |])
                                    [| 0 |]
                                    [ 0 .. fieldBound ] ))
                  $(varE $ mkName (title ++ ".a"))
         |]

    mkUpdate :: String -> (ExpQ -> ExpQ) -> ExpQ
    mkUpdate name update =
      [| bench name $
          nf (\v -> $(appN 5 update [| v |])) $(varE (mkName (name ++ ".a"))) |]

    appN :: Int -> (a -> a) -> a -> a
    appN 0 f = id
    appN k f = appN (k - 1) f . f

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

    genTup mod = tupE [ varE (mkName (mod ++ "." ++ [c])) | c <- defNames ]

    labelA :: Int -> ExpQ
    labelA n = labelE ("a" ++ show n)

    bUpdate :: Int -> ExpQ -> ExpQ
    bUpdate n e = [| B.set $(labelA n) 10 $e |]

    cUpdate :: Int -> ExpQ -> ExpQ
    cUpdate n e = [| C.update $(dyn("C.a" ++ show n)) 10 $e |]

    dUpdate n e = [| DD.replaceN (Proxy :: Proxy $(litT (numTyLit (fromIntegral n)))) $e 10 |]

    -- This library is borked, type infernce fails for even
    -- simple sets.
    -- https://github.com/fumieval/extensible/issues/11
    eUpdate n e = [| set $(labelA n) 10 $e |]

    laUpdate n e = [| La.set $(labelA n) 10 $e |]


    lUpdate n e = [| L.updateList ("a" ++ show n) 10 $e |]

    rawrUpdate n e = [| Control.Lens.set (Rawr.unwrapLens $(labelA n)) (10 :: Int) $e |]

    reUpdate n e = [| $e Re./// (Re.X Re.:& $(dyn ("Re.a" ++ show n)) Re.:= 10) |]

    rUpdate n e = e `recUpdE`  [return (mkName ("R.a" ++ show n), LitE (IntegerL 11) )]

    srUpdate n e = [| SR.set $(dyn ("SR.a" ++ show n)) 10 $e  |]

    vUpdate n e = [| V.rput ($(dyn ("V.a" ++ show n)) V.=:: 10) $e |]

    benchgroup name bs = appsE ([[| bgroup name |], listE bs])

  in listE $
        [ benchgroup "Same-Append"
            [
            mkGrp "C" '(.++) ,
            mkGrp "V" '(<+>) ,
            mkGrp "L"  '(++) ,
            mkGrp "DD"  '(DD././) ,
            mkGrp "SR"  '(SR.++:) ,
            -- Rawr checks for duplicates
            -- Bookkeeper also checks for duplicates
            -- Labels provides no machinery for append
            mkGrp "Re" 'Re.cat ] ]
    ++ [ benchgroup "Lookup"
           [mkLook "B" bLookup ,
           mkLook "C" cLookup,
           mkLook "DD" ddLookup ,
           mkLook "E" eLookup ,
           mkLook "La" laLookup ,
           mkLook "L" lLookup ,
           mkLook "Rawr" rawrLookup  ,
           mkLook "Re" reLookup ,
           mkLook "R" rLookup  ,
           mkLook "SR" srLookup ,
           mkLook "V" vLookup ] ]
   ++ [ benchgroup "Distinct-Append"
           [ -- B doesn't implement append
            mkAppendDiff "C" '(.++) [| C.empty |]
           , mkAppendDiff "DD" '(DD././) [| DD.nil |]
--         , mkAppendDiff "E"  _ [| E.emptyRecord |] (genTup "E")
--         , mkAppendDiff "la"
           , mkAppendDiff "L" '(++) [| [] |]
           -- Only supports up to size 8 records..
           -- , mkAppendDiff "Rawr" 'f [| z |] (genTup "Rawr")
           , mkAppendDiff "Re" 'Re.cat [| renil |]
           -- Normal records can't be appended
           , mkAppendDiff "SR" '(SR.++:) [| SR.rnil |]
           , mkAppendDiff "V" '(<+>) [| V.RNil |]
           ]]
   ++ [
        benchgroup "update"
         [ benchgroup (show n)
                [ mkUpdate "B" (bUpdate n)
                , mkUpdate "C" (cUpdate n)
                , mkUpdate "DD" (dUpdate n)
                , mkUpdate "E" (eUpdate n)
--                https://github.com/fumieval/extensible/issues/11
                , mkUpdate "La" (laUpdate n)
                , mkUpdate "L" (lUpdate n)
                , mkUpdate "Rawr" (rawrUpdate n)
                , mkUpdate "Re" (reUpdate n)
                , mkUpdate "R" (rUpdate n)
                , mkUpdate "SR" (srUpdate n)
                , mkUpdate "V" (vUpdate n) ] | n <- [0.. fieldBound]]]

  )



f = (Rawr.:*:)
z = Rawr.R0
renil = Re.X

srLookup n v = [| SR.get $(dyn ("SR.x" ++ show n)) $(v) |]

myDefn = $((foldr (\n b -> [| (SR.get $(dyn ("SR.a" ++ show n)) SR.a)  + $b |])
                                     [| 0 |]
                                     [ 0 .. fieldBound ] ))

