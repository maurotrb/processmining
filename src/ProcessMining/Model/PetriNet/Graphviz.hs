{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- |
-- Module      :  ProcessMining.Model.PetriNet.Graphviz
-- Copyright   :  Mauro Taraborelli 2015
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Algorithm to convert a Petri Net to dot language.

module ProcessMining.Model.PetriNet.Graphviz
    (
     toDot
    )
where

import           Prelude hiding ((<$>))

import           Data.Text ()
import qualified Data.Text.Lazy ()
import           Text.PrettyPrint.Leijen.Text

import           ProcessMining.Model.PetriNet

-- | Convert a 'PetriNet' to dot language.
--
-- >>> :set -XOverloadedStrings
-- >>> :module + ProcessMining.Log.SimpleLog ProcessMining.Discovery.Alpha
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
-- >>> toDot pn1
-- digraph PetriNet {
--     rankdir=LR;
--     subgraph place {
--         node [shape=circle,fixedsize=true,width=1.5];
--         "input";
--         "output";
--         "p({a},{b,e})";
--         "p({a},{c,e})";
--         "p({b,e},{d})";
--         "p({c,e},{d})";
--     }
--     subgraph toTransitions {
--         node [shape=rect,fixedsize=true,width=1.5];
--         "a";
--         "b";
--         "c";
--         "d";
--         "e";
--     }
--     "input" -> "a";
--     "p({a},{b,e})" -> "b";
--     "p({a},{b,e})" -> "e";
--     "p({a},{c,e})" -> "c";
--     "p({a},{c,e})" -> "e";
--     "p({b,e},{d})" -> "d";
--     "p({c,e},{d})" -> "d";
--     "a" -> "p({a},{b,e})";
--     "a" -> "p({a},{c,e})";
--     "b" -> "p({b,e},{d})";
--     "c" -> "p({c,e},{d})";
--     "d" -> "output";
--     "e" -> "p({b,e},{d})";
--     "e" -> "p({c,e},{d})";
-- }
toDot :: PetriNet -> Doc
toDot pn = text "digraph PetriNet" <+> lbrace
           <> nest 4 (line
                     <> text "rankdir=LR;"
                     <$> text "subgraph place" <+> lbrace
                     <> nest 4 (line
                               <> text "node [shape=circle,fixedsize=true,width=1.5];"
                               <$> vsep (map ((\p -> p <> semi) . dquotes . pretty) ps))
                     <$> rbrace)
           <> nest 4 (line
                     <> text "subgraph toTransitions" <+> lbrace
                     <> nest 4 (line
                               <> text "node [shape=rect,fixedsize=true,width=1.5];"
                               <$> vsep (map ((\t -> t <> semi) . dquotes . pretty) ts))
                     <$> rbrace)
           <> nest 4 (line
                     <> vsep (map ((\f -> f <> semi) . prettyFlowRelationDot) frs))
           <$> rbrace
    where
      ps  = toPlaces pn
      ts  = toTransitions pn
      frs = toFlowRelations pn

prettyFlowRelationDot :: FlowRelation -> Doc
prettyFlowRelationDot (PlaceTransition p t) = dquotes (pretty p) <+> text "->" <+>
                                              dquotes (pretty t)
prettyFlowRelationDot (TransitionPlace t p) = dquotes (pretty t) <+> text "->" <+>
                                              dquotes (pretty p)
