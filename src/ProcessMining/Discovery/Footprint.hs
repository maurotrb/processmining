{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- |
-- Module      :  ProcessMining.Discovery.Footprint
-- Copyright   :  Mauro Taraborelli 2015
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Algorithm to create the footprint of an event log.

module ProcessMining.Discovery.Footprint
    (
      -- * Ordering relations
      Relation(..)
    , Relating(..)
    , Pair(..)
    , relate
    , unrelate
      -- * Footprint
    , Footprint
    , fromSimpleLog
    , relatingEachOther
    , relatingEachSide
    )
where

import           Data.Tuple (swap)
import           Prelude hiding ((<$>))

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.PrettyPrint.Leijen.Text

import           ProcessMining.Log.SimpleLog

-- | Log-base ordering relations.
--
-- Given a "directly follows" relation (@:>>:@) and a "does not follow"
-- relation (@:/>:@) among two activities @a@ and @b@:
--
-- - @a :->: b@ only if @a :>>: b@ and @b :/>: a@
-- - @b :<-: a@ only if @a :>>: b@ and @b :/>: a@
-- - @a :||: b@ only if @a :>>: b@ and @b :>>: a@
-- - @a :##: b@ only if @a :/>: b@ and @b :/>: a@
data Relation = Activity :->: Activity -- ^ Causality. Left to Right.
              | Activity :<-: Activity -- ^ Causality. Right to Left.
              | Activity :||: Activity -- ^ Parallel
              | Activity :##: Activity -- ^ Choice
                deriving (Eq, Ord, Show)

instance Pretty Relation where
    pretty = prettyRelation

-- | Pretty printing for 'Relation'.
prettyRelation :: Relation -> Doc
prettyRelation (left :->: right) = pretty left <+> text "→" <+> pretty right
prettyRelation (left :<-: right) = pretty left <+> text "←" <+> pretty right
prettyRelation (left :||: right) = pretty left <+> text "∥" <+> pretty right
prettyRelation (left :##: right) = pretty left <+> text "⋕" <+> pretty right

-- | Log-base ordering relation types.
data Relating = LeftToRight -- ^ @:->:@
              | RightToLeft -- ^ @:<-:@
              | Parallel    -- ^ @:||:@
              | Choice      -- ^ @:##:@
                deriving (Eq, Ord, Show)

instance Pretty Relating where
    pretty = prettyRelating

-- | Pretty printing for 'Relating'.
prettyRelating :: Relating -> Doc
prettyRelating LeftToRight = text "→"
prettyRelating RightToLeft = text "←"
prettyRelating Parallel    = text "∥"
prettyRelating Choice      = text "⋕"

-- | Log-base ordering relation pair.
data Pair = Pair Activity Activity
            deriving (Eq, Ord, Show)

instance Pretty Pair where
    pretty = prettyPair

-- | Pretty printing for 'Pair'.
prettyPair :: Pair -> Doc
prettyPair (Pair left right) = parens $ cat $ punctuate comma [pretty left, pretty right]

-- | Create a 'Relation' relating a 'Pair'.
--
-- >>> :set -XOverloadedStrings
-- >>> let ab = relate LeftToRight (Pair "a" "b")
-- >>> ab
-- "a" :->: "b"
relate :: Relating -> Pair -> Relation
relate LeftToRight (Pair left right) = left :->: right
relate RightToLeft (Pair left right) = left :<-: right
relate Parallel    (Pair left right) = left :||: right
relate Choice      (Pair left right) = left :##: right

-- | Extract a 'Relating' and a 'Pair' from a 'Relation'.
--
-- >>> :set -XOverloadedStrings
-- >>> let ab = relate LeftToRight (Pair "a" "b")
-- >>> ab
-- "a" :->: "b"
-- >>> unrelate ab
-- (LeftToRight,Pair "a" "b")
unrelate :: Relation -> (Relating, Pair)
unrelate (left :->: right) = (LeftToRight, Pair left right)
unrelate (left :<-: right) = (RightToLeft, Pair left right)
unrelate (left :||: right) = (Parallel,    Pair left right)
unrelate (left :##: right) = (Choice,      Pair left right)

-- | Footprint of a 'SimpleLog'.
data Footprint = Footprint
                 (Set Relation)      -- Ordering relations.
                 (Map Pair Relating) -- Ordering relations, by pair.
                 (Set Activity)      -- Distinct activities in the relations.

instance Show Footprint where
    show (Footprint rs _ _) = show $ Set.toAscList rs

instance Pretty Footprint where
    pretty = prettyFootprint

-- | Pretty printing for 'Footprint'.
prettyFootprint :: Footprint -> Doc
prettyFootprint (Footprint _ rsp as) = header <$> vsep rows
    where
      header = space <+> sep (map pretty $ Set.toAscList as)
      rows   = [ pretty left <+>
                 hsep [ pretty (Map.findWithDefault Choice (Pair left right) rsp)
                      | right <- Set.toAscList as ]
               | left <- Set.toAscList as ]

-- | Finds the 'Footprint' of a 'Simplelog'
--
-- >>> :set -XOverloadedStrings
-- >>> let abcd = addActivity "d" $ addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let acbd = addActivity "d" $ addActivity "b" $ addActivity "c" $ addActivity "a" $ emptyTrace
-- >>> let aed  = addActivity "d" $ addActivity "e" $ addActivity "a" $ emptyTrace
-- >>> let l1 = addTrace aed $ addTrace acbd $ addTrace abcd $ emptySimpleLog
-- >>> l1
-- [<"a","b","c","d">,<"a","c","b","d">,<"a","e","d">]
-- >>> fromSimpleLog l1
-- ["a" :->: "b","a" :->: "c","a" :->: "e","b" :->: "d","c" :->: "d","e" :->: "d","b" :<-: "a","c" :<-: "a","d" :<-: "b","d" :<-: "c","d" :<-: "e","e" :<-: "a","b" :||: "c","c" :||: "b","a" :##: "a","a" :##: "d","b" :##: "b","b" :##: "e","c" :##: "c","c" :##: "e","d" :##: "a","d" :##: "d","e" :##: "b","e" :##: "c","e" :##: "e"]
fromSimpleLog :: SimpleLog -> Footprint
fromSimpleLog sl = Footprint (Set.fromList $ map (uncurry relate . swap) $ Map.toList relationsAll)
                             relationsAll
                             (Set.fromList activities)
    where
      activities   = distinctActivities sl
      traces       = distinctTraces sl
      pairs        = concatMap (relatedPairs . trace) traces                  -- Creates pairs with a "follows" relation
      relatings    = map (classify $ Set.fromList pairs) pairs                -- Finds out which type of relation (LeftToRight or Parallel)
      relations    = concatMap completeCausality $ zip pairs relatings        -- Pairs up and complete with RightToLeft relations
      relationsAll = Map.fromList relations `Map.union`
                     Map.fromList [ (Pair left right, Choice)                 -- Create Choice relations for
                                  | left <- activities, right <- activities ] -- all activities combinations

-- | Returns pairs of related activities
relatedPairs :: [Activity] -> [Pair]
relatedPairs [] = []
relatedPairs as = zipWith Pair (init as) (tail as)

-- | Classifies "directly follows" relations
classify :: Set Pair -> Pair -> Relating
classify ps (Pair left right) | Pair left right `Set.member`    ps &&
                                Pair right left `Set.notMember` ps = LeftToRight
                              | Pair left right `Set.notMember` ps &&
                                Pair right left `Set.member`    ps = RightToLeft
                              | Pair left right `Set.member`    ps &&
                                Pair right left `Set.member`    ps = Parallel
                              | otherwise                          = Choice

-- | Completes left-to-right causality with right-to-left one
completeCausality :: (Pair, Relating) -> [(Pair, Relating)]
completeCausality lr@(Pair left right, LeftToRight) = [lr, (Pair right left, RightToLeft)]
completeCausality other                             = [other]

-- | Tests if a list of activities are relating each other with the same type of relation.
--
-- >>> :set -XOverloadedStrings
-- >>> let abcd = addActivity "d" $ addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let acbd = addActivity "d" $ addActivity "b" $ addActivity "c" $ addActivity "a" $ emptyTrace
-- >>> let aed  = addActivity "d" $ addActivity "e" $ addActivity "a" $ emptyTrace
-- >>> let l1 = addTrace aed $ addTrace acbd $ addTrace abcd $ emptySimpleLog
-- >>> let fp = fromSimpleLog l1
-- >>> relatingEachOther fp Choice ["c","e"]
-- True
-- >>> relatingEachOther fp Choice ["c","d"]
-- False
relatingEachOther :: Footprint -> Relating -> [Activity] -> Bool
relatingEachOther (Footprint _ rsp _) r as = Map.isSubmapOf eachOther rsp
    where
      eachOther  = Map.fromList [ (Pair left right, r) | left <- as, right <- as ]

-- | Tests if activities in two lists are relating each other with the same type of relation.
--
-- >>> :set -XOverloadedStrings
-- >>> let abcd = addActivity "d" $ addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let acbd = addActivity "d" $ addActivity "b" $ addActivity "c" $ addActivity "a" $ emptyTrace
-- >>> let aed  = addActivity "d" $ addActivity "e" $ addActivity "a" $ emptyTrace
-- >>> let l1 = addTrace aed $ addTrace acbd $ addTrace abcd $ emptySimpleLog
-- >>> let fp = fromSimpleLog l1
-- >>> relatingEachSide fp LeftToRight ["a"] ["b","c","e"]
-- True
-- >>> relatingEachSide fp LeftToRight ["a"] ["b","c","d"]
-- False
relatingEachSide :: Footprint -> Relating -> [Activity] -> [Activity] -> Bool
relatingEachSide (Footprint _ rsp _) r lefts rights = Map.isSubmapOf eachOther rsp
    where
      eachOther  = Map.fromList [ (Pair left right, r) | left <- lefts, right <- rights ]
