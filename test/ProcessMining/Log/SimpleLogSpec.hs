{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  ProcessMining.Log.SimpleEventLogSpec
-- Copyright   :  Mauro Taraborelli 2015
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Spec tests for process mining libraries
--
-- Internal module.

module ProcessMining.Log.SimpleLogSpec where

import           Data.Text()
import           Test.Hspec
import           Test.QuickCheck

import           ProcessMining.Log.SimpleLog

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "trace" $
       it "returns activities in the same order added" $ property $ \as ->
           trace (foldl (flip addActivity) emptyTrace $ extractActivity as) == extractActivity as

  describe "empty simple log" $
       context "when added an empty trace" $
               it "remains an empty simple log" $
                  addTrace emptyTrace emptySimpleLog `shouldBe` emptySimpleLog

extractActivity :: [AlphaActivity] -> [Activity]
extractActivity ((AlphaActivity a):as) = a:extractActivity as
extractActivity []                     = []

newtype AlphaActivity = AlphaActivity Activity
                      deriving Show

instance Arbitrary AlphaActivity where
    arbitrary = elements $ map (AlphaActivity . Activity) ["a", "b", "c", "d", "e", "f", "g"]
