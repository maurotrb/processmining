{-# LANGUAGE OverloadedStrings #-}

module Scratch where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.PrettyPrint.Leijen.Text

import ProcessMining.Discovery.Alpha
import ProcessMining.Discovery.Footprint
import ProcessMining.Log.SimpleLog
import ProcessMining.Model.PetriNet
import ProcessMining.Model.PetriNet.Graphviz

-- Events
a = Activity "a"
b = Activity "b"
c = Activity "c"
d = Activity "d"
e = Activity "e"
f = Activity "f"

-- L1 = [(a,b,c,d)3,(a,c,b,d)2,(a,e,d)]
abcd = addActivity d $ addActivity c $ addActivity b $ addActivity a $ emptyTrace
acbd = addActivity d $ addActivity b $ addActivity c $ addActivity a $ emptyTrace
aed  = addActivity d $ addActivity e $ addActivity a $ emptyTrace
l1 = addTrace aed $
     addTrace acbd $ addTrace acbd $
     addTrace abcd $ addTrace abcd $ addTrace abcd $
     emptySimpleLog

pn1 = alpha l1

pn1Dot = toDot pn1
writePn1Dot = writeFile "pn1.dot" (show pn1Dot)
-- create a png with:> dot pn1.dot -Tpng -opn1.png

-- L2 = [(a,b,c,d)3,(a,c,b,d)4,(a,b,c,e,f,b,c,d)2,(a,b,c,e,f,c,b,d),(a,c,b,e,f,b,c,d)2,(a,c,b,e,f,b,c,e,f,c,b,d)]
-- abcd and acbd from L1
abcefbcd     = addActivity d $ addActivity c $ addActivity b $ addActivity f $ addActivity e $
               addActivity c $ addActivity b $ addActivity a $ emptyTrace
abcefcbd     = addActivity d $ addActivity b $ addActivity c $ addActivity f $ addActivity e $
               addActivity c $ addActivity b $ addActivity a $ emptyTrace
acbefbcd     = addActivity d $ addActivity c $ addActivity b $ addActivity f $ addActivity e $
               addActivity b $ addActivity c $ addActivity a $ emptyTrace
acbefbcefcbd = addActivity d $ addActivity b $ addActivity c $ addActivity f $ addActivity e $
               addActivity c $ addActivity b $ addActivity f $ addActivity e $ addActivity b $ addActivity c $ addActivity a $ emptyTrace
l2 = addTrace acbefbcefcbd $
     addTrace acbefbcd $ addTrace acbefbcd $
     addTrace abcefcbd $
     addTrace abcefbcd $ addTrace abcefbcd $
     addTrace acbd $ addTrace acbd $ addTrace acbd $ addTrace acbd $
     addTrace abcd $ addTrace abcd $ addTrace abcd $
     emptySimpleLog

pn2 = alpha l2

pn2Dot = toDot pn2
writePn2Dot = writeFile "pn2.dot" (show pn2Dot)
-- create a png with:> dot pn2.dot -Tpng -opn2.png

-- L5 = [(a,b,e,f)2,(a,b,e,c,d,b,f)3,(a,b,c,e,d,b,f)2,(a,b,c,d,e,b,f)4,(a,e,b,c,d,b,f)3]
abef    = addActivity f $ addActivity e $ addActivity b $ addActivity a $ emptyTrace
abecdbf = addActivity f $ addActivity b $ addActivity d $ addActivity c $ addActivity e
          $ addActivity b $ addActivity a $ emptyTrace
abcedbf = addActivity f $ addActivity b $ addActivity d $ addActivity e $ addActivity c
          $ addActivity b $ addActivity a $ emptyTrace
abcdebf = addActivity f $ addActivity b $ addActivity e $ addActivity d $ addActivity c
          $ addActivity b $ addActivity a $ emptyTrace
aebcdbf = addActivity f $ addActivity b $ addActivity d $ addActivity c $ addActivity b
          $ addActivity e $ addActivity a $ emptyTrace
l5 = addTrace aebcdbf $ addTrace aebcdbf $ addTrace aebcdbf $
     addTrace abcdebf $ addTrace abcdebf $ addTrace abcdebf $ addTrace abcdebf $
     addTrace abcedbf $ addTrace abcedbf $
     addTrace abecdbf $ addTrace abecdbf $ addTrace abecdbf $
     addTrace abef $ addTrace abef $
     emptySimpleLog

pn5 = alpha l5

pn5Dot = toDot pn5
writePn5Dot = writeFile "pn5.dot" (show pn5Dot)
-- create a png with:> dot pn5.dot -Tpng -opn5.png
