{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- |
-- Module      :  ProcessMining.Log.SimpleEventLog
-- Copyright   :  Mauro Taraborelli 2015
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple event log.

module ProcessMining.Log.SimpleLog
    (
      -- * Activity
      Activity(..)
      -- * Trace
    , Trace
    , emptyTrace
    , addActivity
    , addActivities
    , trace
      -- * SimpleLog
    , SimpleLog
    , emptySimpleLog
    , addTrace
    , addTraces
    , simplelog
    , distinctTraces
    , distinctActivities
    , firstActivities
    , lastActivities
    )
where

import           Data.List (intercalate)

import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import           GHC.Exts (IsString, fromString)
import           Text.PrettyPrint.Leijen.Text

-- | Activity name.
-- Activities are the building blocks of 'SimpleLog'.
--
-- It is a wrapper around a 'Data.Text.Text' value.
--
-- >>> :set -XOverloadedStrings
-- >>> let a = "a" :: Activity
-- >>> a
-- "a"
newtype Activity = Activity { unActivity :: Text }
    deriving (Eq, Ord)

instance Show Activity where
    show (Activity a) = show a

instance IsString Activity where
    fromString = Activity . Text.pack

instance Pretty Activity where
    pretty = prettyActivity

-- | Pretty printing for 'Activity'.
prettyActivity :: Activity -> Doc
prettyActivity (Activity a) = text $ TextL.fromStrict a

-- | A Trace is a finite sequence of activities.
--
-- >>> :set -XOverloadedStrings
-- >>> let t = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> t
-- <"a","b","c">
data Trace = Trace
             (DList Activity) -- Trace of activities
             (Set Activity)   -- Distinct activities
             Activity         -- First activity
             Activity         -- Last activity
           | EmptyTrace deriving (Eq, Ord)

instance Show Trace where
    show (Trace t _ _ _) = "<" ++ (intercalate "," . map show . DList.toList) t ++ ">"
    show EmptyTrace      = "Empty trace"

instance Pretty Trace where
    pretty = prettyTrace

-- | Pretty printer for 'Trace'.
prettyTrace :: Trace -> Doc
prettyTrace (Trace t _ _ _) = langle <>
                              cat (punctuate comma (map pretty (DList.toList t))) <>
                              rangle
prettyTrace EmptyTrace      = text "Empty trace"

-- | Creates an empty 'Trace'.
--
-- >>> let t = emptyTrace
-- >>> t
-- Empty trace
emptyTrace :: Trace
emptyTrace = EmptyTrace

-- | Adds an 'Activity' to a 'Trace'.
--
-- >>> :set -XOverloadedStrings
-- >>> let t = addActivity "a" $ emptyTrace
-- >>> t
-- <"a">
addActivity :: Activity -> Trace -> Trace
addActivity a (Trace t as fa _) = Trace (t `DList.snoc` a)  (Set.insert a as) fa a
addActivity a EmptyTrace        = Trace (DList.singleton a) (Set.singleton a) a  a

-- | Adds a list of 'Activity' to a 'Trace'
addActivities :: [Activity] -> Trace -> Trace
addActivities as t = foldl (flip addActivity) t as

-- | Returns the 'Activity' composing the 'Trace'.
--
-- >>> :set -XOverloadedStrings
-- >>> let t = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> trace t
-- ["a","b","c"]
trace :: Trace -> [Activity]
trace (Trace t _ _ _) = DList.toList t
trace EmptyTrace      = []

-- | Simple log.
-- A simple event log is a multi-set of 'Trace's.
--
-- >>> :set -XOverloadedStrings
-- >>> let t1 = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let t2 = addActivity "d" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let s = addTrace t2 $ addTrace t1 $ addTrace t1 $ emptySimpleLog
-- >>> s
-- [<"a","b","c">2,<"a","b","d">]
data SimpleLog = SimpleLog
                 (MultiSet Trace) -- Traces
                 (Set Activity)   -- Distinct activities
                 (Set Activity)   -- First activities
                 (Set Activity)   -- Last activities
               | EmptySimpleLog deriving (Eq)

instance Show SimpleLog where
    show (SimpleLog l _ _ _) =
        let
            showTraceWithOccur (t,o) = if o == 1 then show t else show t ++ show o
        in
          "[" ++ (intercalate "," . map showTraceWithOccur . MultiSet.toAscOccurList) l ++ "]"
    show EmptySimpleLog      = "Empty log"

instance Pretty SimpleLog where
    pretty = prettySimpleLog

-- | Pretty printer for 'SimpleLog'.
prettySimpleLog :: SimpleLog -> Doc
prettySimpleLog (SimpleLog l _ _ _) =
    let
        textTraceWithOccur (t,o) =  if o == 1 then pretty t else pretty t <> text (TextL.pack $ show o)
    in
      brackets $ cat (punctuate comma (map textTraceWithOccur $ MultiSet.toAscOccurList l))
prettySimpleLog EmptySimpleLog      = text "Empty log"

-- | Creates an empty 'SimpleLog'.
--
-- >>> let s = emptySimpleLog
-- >>> s
-- Empty log
emptySimpleLog :: SimpleLog
emptySimpleLog = EmptySimpleLog

-- | Adds a 'Trace' to a 'SimpleLog'.
--
-- >>> :set -XOverloadedStrings
-- >>> let t = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let s = addTrace t $ emptySimpleLog
-- >>> s
-- [<"a","b","c">]
addTrace :: Trace -> SimpleLog -> SimpleLog
addTrace tr@(Trace _ as fa la) EmptySimpleLog            = SimpleLog
                                                           (MultiSet.singleton tr)
                                                           as
                                                           (Set.singleton fa)
                                                           (Set.singleton la)
addTrace tr@(Trace _ as fa la) (SimpleLog l aas fas las) = SimpleLog
                                                           (MultiSet.insert tr l)
                                                           (aas `Set.union` as)
                                                           (Set.insert fa fas)
                                                           (Set.insert la las)
addTrace EmptyTrace            sl                        = sl

-- | Adds a list of 'Trace' to a 'SimpleLog'.
addTraces :: [Trace] -> SimpleLog -> SimpleLog
addTraces ts sl = foldr addTrace sl ts

-- | Returns the 'Trace's contained in a 'SimpleLog'.
--
-- >>> :set -XOverloadedStrings
-- >>> let t1 = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let t2 = addActivity "d" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let s = addTrace t2 $ addTrace t1 $ addTrace t1 $ emptySimpleLog
-- >>> simplelog s
-- [<"a","b","c">,<"a","b","c">,<"a","b","d">]
simplelog :: SimpleLog -> [Trace]
simplelog (SimpleLog l _ _ _) = MultiSet.elems l
simplelog EmptySimpleLog      = []

-- | Returns the distinct 'Trace's contained in a 'SimpleLog'.
--
-- >>> :set -XOverloadedStrings
-- >>> let t1 = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let t2 = addActivity "d" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let s = addTrace t2 $ addTrace t1 $ addTrace t1 $ emptySimpleLog
-- >>> distinctTraces s
-- [<"a","b","c">,<"a","b","d">]
distinctTraces :: SimpleLog -> [Trace]
distinctTraces (SimpleLog l _ _ _) = MultiSet.distinctElems l
distinctTraces EmptySimpleLog      = []

-- | Returns the distinct 'Activity' contained in a 'SimpleLog'.
--
-- >>> :set -XOverloadedStrings
-- >>> let t1 = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let t2 = addActivity "d" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let s = addTrace t2 $ addTrace t1 $ addTrace t1 $ emptySimpleLog
-- >>> distinctActivities s
-- ["a","b","c","d"]
distinctActivities :: SimpleLog -> [Activity]
distinctActivities (SimpleLog _ as _ _) = Set.toAscList as
distinctActivities EmptySimpleLog       = []

-- | Returns the first 'Activity' of 'Trace's contained in a 'SimpleLog'.
--
-- >>> :set -XOverloadedStrings
-- >>> let t1 = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let t2 = addActivity "d" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let s = addTrace t2 $ addTrace t1 $ addTrace t1 $ emptySimpleLog
-- >>> firstActivities s
-- ["a"]
firstActivities :: SimpleLog -> [Activity]
firstActivities (SimpleLog _ _ fas _) = Set.toAscList fas
firstActivities EmptySimpleLog        = []

-- | Returns the last 'Activity' of 'Trace's contained in a 'SimpleLog'.
--
-- >>> :set -XOverloadedStrings
-- >>> let t1 = addActivity "c" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let t2 = addActivity "d" $ addActivity "b" $ addActivity "a" $ emptyTrace
-- >>> let s = addTrace t2 $ addTrace t1 $ addTrace t1 $ emptySimpleLog
-- >>> lastActivities s
-- ["c","d"]
lastActivities :: SimpleLog -> [Activity]
lastActivities (SimpleLog _ _ _ las) = Set.toAscList las
lastActivities EmptySimpleLog        = []
