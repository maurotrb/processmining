{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- |
-- Module      :  ProcessMining.Discovery.Alpha
-- Copyright   :  Mauro Taraborelli 2015
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Alpha algorithm to discover a Petri Net from a event log.

module ProcessMining.Discovery.Alpha
    (
      -- * Alpha algorithm
      alpha
    )
where

import           Data.List (subsequences)

import qualified Data.Set as Set
import           Data.Text ()
import qualified Data.Text as Text
import           Text.PrettyPrint.Leijen.Text

import           ProcessMining.Discovery.Footprint
import           ProcessMining.Log.SimpleLog
import           ProcessMining.Model.PetriNet

-- | Alpha algorithm to discover a 'PetriNet' from a 'SimpleLog'.
--
-- >>> :set -XOverloadedStrings
-- >>> let a = "a" :: Activity
-- >>> let b = "b" :: Activity
-- >>> let c = "c" :: Activity
-- >>> let d = "d" :: Activity
-- >>> let e = "e" :: Activity
-- >>> let abcd = addActivities [a,b,c,d] emptyTrace
-- >>> let acbd = addActivities [a,c,b,d] emptyTrace
-- >>> let aed  = addActivities [a,e,d] emptyTrace
-- >>> let l1 = addTraces [aed,acbd,acbd,abcd,abcd,abcd] emptySimpleLog
-- >>> let pn1 = alpha l1
-- >>> toPlaces pn1
-- ["input","output","p({a},{b,e})","p({a},{c,e})","p({b,e},{d})","p({c,e},{d})"]
-- >>> toTransitions pn1
-- ["a","b","c","d","e"]
-- >>> toFlowRelations pn1
-- [PlaceTransition "input" "a",PlaceTransition "p({a},{b,e})" "b",PlaceTransition "p({a},{b,e})" "e",PlaceTransition "p({a},{c,e})" "c",PlaceTransition "p({a},{c,e})" "e",PlaceTransition "p({b,e},{d})" "d",PlaceTransition "p({c,e},{d})" "d",TransitionPlace "a" "p({a},{b,e})",TransitionPlace "a" "p({a},{c,e})",TransitionPlace "b" "p({b,e},{d})",TransitionPlace "c" "p({c,e},{d})",TransitionPlace "d" "output",TransitionPlace "e" "p({b,e},{d})",TransitionPlace "e" "p({c,e},{d})"]
alpha :: SimpleLog -> PetriNet
alpha sl = addFlowRelations fl $ addTransitions tl' $ addPlaces pl emptyPetriNet --(pl,fl,tlt)
    where
      fp  = fromSimpleLog sl                         -- footprint
      tl = distinctActivities sl                     -- Tl
      ti = firstActivities sl                        -- Ti
      to = lastActivities sl                         -- To
      xl = [ (a, b)                                  -- { Xl = (A, B)
           | a <- subsequences tl                    --        | A ⊆ Tl
           , (not . null) a                          --        ⋀ A ≠ ∅
           , b <- subsequences tl                    --        ⋀ B ⊆ Tl
           , (not . null) b                          --        ⋀ B ≠ ∅
           , relatingEachSide fp LeftToRight a b     --        ⋀ ∀a∊A ∀b∊B a→b
           , relatingEachOther fp Choice a           --        ⋀ ∀a1,a2∊A a1⋕a2
           , relatingEachOther fp Choice b ]         --        ⋀ ∀b1,b2∊A b1⋕b2 }
      yl = [ (a, b)                                  -- { Yl = (A, B) ∈ Xl
           | (a, b)   <- xl                          --
           , null [ (a', b')                         --        | ∀(A',B')∊Xl
                  | (a', b') <- xl                   --
                  , Set.isSubsetOf (Set.fromList a)  --          A⊆A'
                                   (Set.fromList a') --
                  , Set.isSubsetOf (Set.fromList b)  --        ⋀ B⊆B'
                                   (Set.fromList b') --
                  , (a', b') /= (a, b)               --
                  ]                                  --        ⇒ (A,B)=(A',B') }
           ]
      pl = [ Place (Text.pack $ show $ prettyPlaceFromPairs ab) | ab <- yl ] ++
           ["input", "output"] :: [Place]
      fl = [ TransitionPlace (Transition $ unActivity a) (Place (Text.pack $ show $ prettyPlaceFromPairs ab))
           | ab@(as, _) <- yl, a <- as ] ++
           [ PlaceTransition (Place (Text.pack $ show $ prettyPlaceFromPairs ab)) (Transition $ unActivity b)
           | ab@(_, bs) <- yl, b <- bs ] ++
           [ PlaceTransition "input" (Transition $ unActivity t)
           | t <- ti ] ++
           [ TransitionPlace (Transition $ unActivity t) "output"
           | t <- to ] :: [FlowRelation]
      tl'= map (Transition . unActivity) tl

prettyPlaceFromPairs :: ([Activity],[Activity]) -> Doc
prettyPlaceFromPairs (lefts,rights) = text "p"
                                    <> parens (braces (cat $ punctuate comma $ map pretty lefts)
                                              <> comma
                                              <> braces (cat $ punctuate comma $ map pretty rights))
