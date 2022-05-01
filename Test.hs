{-# LANGUAGE OverloadedStrings #-}
-- (C) 2022: Hans Georg Schaathun <hg+gamer@schaathun.net>

module Main where

import System.IO as IO
import qualified Data.Text.IO as DTIO
import qualified Data.Text.Lazy.IO as DTLIO
import Control.Monad
import qualified Data.Text.Lazy as  T
import Swish.RDF.Formatter.Turtle
import Swish.RDF.Graph
import Data.Maybe
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import Data.Text.Encoding
import Network.URI
import ArM.Resources


import qualified Data.ByteString.Lazy as BS
import ArM.Load
import ArM.Resources as AR
import ArM.Character
import ArM.Types.Character as TC
import ArM.STM
import ArM.JSON
import Data.Aeson.Encode.Pretty
import Data.Aeson

testCharacter = "armchar:cieran"

s = "{\n     \"arm:hasDescription\": \"<p>You automatically master every spell that you learn. All your spells start with a score of 1 in the corresponding Ability. You may choose a different special ability for every spell you have. Further, all experience points you put into Spell Mastery Abilities are doubled.</p>  \",\n     \"arm:hasLabel\": \"Flawless Magic\",\n     \"arm:hasReference\": \"[ArM5:42]\",\n        \"arm:prefixedid\": \"_:118\", \n      \"arm:traitClass\": { \n     \"prefixedid\": \"armr:flawlessMagic\" \n   } \n } \n"

-- main :: IO ()
main = do 

     (g',schema,res) <- getGraph AR.characterFile AR.armFile AR.resourceFile
     let g = merge g' schema
     contents <- BS.readFile "Test/adv.json"
     let  adv' :: Maybe Advancement
          adv' = decode contents
     print adv'
     let adv = fromJust adv'
     let  advg0 = makeRDFGraph $ adv
     let  advg = merge schema $ makeRDFGraph $ adv
     print "GRAPH advg0"
     DTIO.putStrLn $ formatGraphAsText $ advg0
     print "GRAPH advg (New advancement with schema)"
     DTIO.putStrLn $ formatGraphAsText $ advg
     print "persistGraph"
     DTIO.putStrLn $ formatGraphAsText $ persistGraph $ advg
     print "persistGraph'"
     DTIO.putStrLn $ formatGraphAsText $ persistGraph' $ advg
     print "end"
     let adv0 = TC.fromRDFGraph g (TC.rdfid adv) :: TC.Advancement
     print adv0
     let g0 = TC.makeRDFGraph adv0
     print "Advancement graph extracted"
     DTIO.putStrLn $ formatGraphAsText $ g0
     let gg0 = delete g0 g
     print "Graph after delete"
     DTIO.putStrLn $ formatGraphAsText $ gg0
     let gg1 = merge gg0 advg
     print "New Graph"
     DTIO.putStrLn $ formatGraphAsText $ gg1
     print "Extract advancement from new graph"
     let adv1 = TC.fromRDFGraph gg1 (TC.rdfid adv) :: TC.Advancement
     print adv1



