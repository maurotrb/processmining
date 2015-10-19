module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "src/ProcessMining/Discovery/Alpha"
               , "src/ProcessMining/Discovery/Footprint"
               , "src/ProcessMining/Log/SimpleLog"
               , "src/ProcessMining/Model/PetriNet" ]
