{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- |
-- Module      :  ProcessMining.Model.PetriNet
-- Copyright   :  Mauro Taraborelli 2015
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Petri Net model.

module ProcessMining.Model.PetriNet
    (
      -- * Place
      Place(..)
      -- * Transition
    , Transition(..)
      -- * FlowRelation
    , FlowRelation(..)
      -- * PetriNet
    , PetriNet
    , toPlaces
    , toTransitions
    , toFlowRelations
    , emptyPetriNet
    , addPlace
    , addTransition
    , addFlowRelation
    , addPlaces
    , addTransitions
    , addFlowRelations
    )
where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import           GHC.Exts (IsString, fromString)
import           Text.PrettyPrint.Leijen.Text

-- | Place in a 'PetriNet'.
--
-- It is a wrapper around a 'Data.Text.Text' value.
--
-- >>> :set -XOverloadedStrings
-- >>> let a = "a" :: Place
-- >>> a
-- "a"
newtype Place = Place { unPlace :: Text }
    deriving (Eq, Ord)

instance Show Place where
    show (Place p) = show p

instance IsString Place where
    fromString = Place . Text.pack

instance Pretty Place where
    pretty = prettyPlace

-- | Pretty printing for 'Place'.
prettyPlace :: Place -> Doc
prettyPlace (Place p) = text $ TextL.fromStrict p

-- | Transition in a 'PetriNet'.
--
-- It is a wrapper around a 'Data.Text.Text' value.
--
-- >>> :set -XOverloadedStrings
-- >>> let c1 = "c1" :: Transition
-- >>> c1
-- "c1"
newtype Transition = Transition { unTransition :: Text }
    deriving (Eq, Ord)

instance Show Transition where
    show (Transition t) = show t

instance IsString Transition where
    fromString = Transition . Text.pack

instance Pretty Transition where
    pretty = prettyTransition

-- | Pretty printing for 'Transition'.
prettyTransition :: Transition -> Doc
prettyTransition (Transition t) = text $ TextL.fromStrict t

-- | Flow relation in a 'PetriNet'.
--
-- >>> :set -XOverloadedStrings
-- >>> let a = "a" :: Place
-- >>> let b = "b" :: Place
-- >>> let c1 = "c1" :: Transition
-- >>> let ac1 = PlaceTransition a c1
-- >>> ac1
-- PlaceTransition "a" "c1"
-- >>> let c1b = TransitionPlace c1 b
-- >>> c1b
-- TransitionPlace "c1" "b"
data FlowRelation = PlaceTransition Place Transition
                  | TransitionPlace Transition Place
                    deriving (Eq, Ord, Show)

-- | PetriNet.
--
-- >>> :set -XOverloadedStrings
-- >>> let a = "a" :: Place
-- >>> let b = "b" :: Place
-- >>> let c1 = "c1" :: Transition
-- >>> let ac1 = PlaceTransition a c1
-- >>> let c1b = TransitionPlace c1 b
-- >>> let pn = addFlowRelations [ac1,c1b] $ addTransition c1 $ addPlaces [a,b] emptyPetriNet
-- >>> pn
-- (["a","b"],["c1"],[PlaceTransition "a" "c1",TransitionPlace "c1" "b"])
data PetriNet = PetriNet
                (Set Place)        -- Places
                (Set Transition)   -- Transitions
                (Set FlowRelation) -- Flow relations
              | EmptyPetriNet
                deriving (Eq)

instance Show PetriNet where
    show (PetriNet ps ts frs) = show (Set.toAscList ps
                                     ,Set.toAscList ts
                                     ,Set.toAscList frs)
    show EmptyPetriNet        = "Empty petri net"

-- | Returns 'PetriNet' places
--
-- >>> :set -XOverloadedStrings
-- >>> let a = "a" :: Place
-- >>> let b = "b" :: Place
-- >>> let c1 = "c1" :: Transition
-- >>> let ac1 = PlaceTransition a c1
-- >>> let c1b = TransitionPlace c1 b
-- >>> let pn = addFlowRelations [ac1,c1b] $ addTransition c1 $ addPlaces [a,b] emptyPetriNet
-- >>> toPlaces pn
-- ["a","b"]
toPlaces :: PetriNet -> [Place]
toPlaces (PetriNet ps _ _) = Set.toAscList ps
toPlaces EmptyPetriNet     = []

-- | Returns 'PetriNet' transitions
--
-- >>> :set -XOverloadedStrings
-- >>> let a = "a" :: Place
-- >>> let b = "b" :: Place
-- >>> let c1 = "c1" :: Transition
-- >>> let ac1 = PlaceTransition a c1
-- >>> let c1b = TransitionPlace c1 b
-- >>> let pn = addFlowRelations [ac1,c1b] $ addTransition c1 $ addPlaces [a,b] emptyPetriNet
-- >>> toTransitions pn
-- ["c1"]
toTransitions :: PetriNet -> [Transition]
toTransitions (PetriNet _ ts _) = Set.toAscList ts
toTransitions EmptyPetriNet     = []

-- | Returns 'PetriNet' flow relations
--
-- >>> :set -XOverloadedStrings
-- >>> let a = "a" :: Place
-- >>> let b = "b" :: Place
-- >>> let c1 = "c1" :: Transition
-- >>> let ac1 = PlaceTransition a c1
-- >>> let c1b = TransitionPlace c1 b
-- >>> let pn = addFlowRelations [ac1,c1b] $ addTransition c1 $ addPlaces [a,b] emptyPetriNet
-- >>> toFlowRelations pn
-- [PlaceTransition "a" "c1",TransitionPlace "c1" "b"]
toFlowRelations :: PetriNet -> [FlowRelation]
toFlowRelations (PetriNet _ _ frs) = Set.toAscList frs
toFlowRelations EmptyPetriNet      = []

-- | Creates an empty 'PetriNet'
emptyPetriNet :: PetriNet
emptyPetriNet = EmptyPetriNet

-- | Adds a 'Place' to a 'PetriNet'
addPlace :: Place -> PetriNet -> PetriNet
addPlace p (PetriNet ps ts frs) = PetriNet (Set.insert p ps) ts        frs
addPlace p EmptyPetriNet        = PetriNet (Set.singleton p) Set.empty Set.empty

-- | Adds a 'Transition' to a 'PetriNet'
addTransition :: Transition -> PetriNet -> PetriNet
addTransition t (PetriNet ps ts frs) = PetriNet ps        (Set.insert t ts) frs
addTransition t EmptyPetriNet        = PetriNet Set.empty (Set.singleton t) Set.empty

-- | Adds a 'FlowRelation' to a 'PetriNet'
-- Invariant! fr must be of ps or ts
addFlowRelation :: FlowRelation -> PetriNet -> PetriNet
addFlowRelation fr (PetriNet ps ts frs) = PetriNet ps        ts        (Set.insert fr frs)
addFlowRelation fr EmptyPetriNet        = PetriNet Set.empty Set.empty (Set.singleton fr)

-- | Adds a list of 'Place' to a 'PetriNet'
addPlaces :: [Place] -> PetriNet -> PetriNet
addPlaces ps pn = foldr addPlace pn ps

-- | Adds a list of 'Transition' to a 'PetriNet'
addTransitions :: [Transition] -> PetriNet -> PetriNet
addTransitions ts pn = foldr addTransition pn ts

-- | Adds a list of 'FlowRelation' to a 'PetriNet'
addFlowRelations :: [FlowRelation] -> PetriNet -> PetriNet
addFlowRelations frs pn = foldr addFlowRelation pn frs
