{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.CharacterMap
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- A Map to store character sheets.
-- Each character sheet is stored as one RDFGraph.
--
-----------------------------------------------------------------------------
module ArM.CharacterMap ( CharacterRecord(..)
                        , insertListS
                        , empty 
                        , ArM.CharacterMap.lookup 
                        ) where

import qualified Data.Map as M
import qualified Swish.RDF.Graph as G
import qualified ArM.Character as C
import qualified ArM.Rules as R
import           Data.Maybe (fromJust)
import ArM.Resources 

type CharacterMap = M.Map CharacterKey CharacterRecord
data CharacterKey = CharacterKey {
            keyYear :: Int,
            keySeason :: String,
            keyChar :: String } deriving (Ord,Eq,Show)
data CharacterRecord = CharacterRecord G.RDFGraph

-- | Prepare character sheet with a given schema and insert into the map 
insertS :: G.RDFGraph -> CharacterMap -> C.CharacterSheet -> CharacterMap
insertS schema cmap cs = M.insert (getKey cs) cr cmap
    where cr = CharacterRecord $ R.prepareRecord schema $ C.makeRDFGraph cs

-- | Add a character sheet into the map (not used).
insert :: CharacterMap -> C.CharacterSheet -> CharacterMap
insert cmap cs = M.insert (getKey cs) cr cmap
    where cr = CharacterRecord $ C.makeRDFGraph cs

getKey :: C.CharacterSheet -> CharacterKey
getKey cs = CharacterKey { keyYear = case (C.csYear cs) of
                                Nothing -> 0
                                (Just y) -> y,
                           keySeason = (C.csSeason cs),
                           keyChar = show $ C.csID cs }

seasonString Nothing = ""
seasonString (Just x ) | x == springLabel = "Spring"
                   | x == summerLabel = "Summer"
                   | x == autumnLabel = "Autumn"
                   | x == winterLabel = "Winter"
                   | otherwise  = "NoSeason"

-- | Add a list of character sheets into the map (not used).
insertList :: CharacterMap -> [C.CharacterSheet] -> CharacterMap
insertList cmap cs = foldl insert cmap cs

-- | Prepare a list of character sheets with a given schema and insert into the map 
insertListS :: G.RDFGraph -> CharacterMap -> [C.CharacterSheet] -> CharacterMap
insertListS schema cmap cs = foldl (insertS schema) cmap cs

empty :: CharacterMap
empty = M.empty
lookup :: CharacterMap -> String -> String -> Int -> Maybe CharacterRecord
lookup cmap c s y = M.lookup ck cmap
   where ck = CharacterKey { keyYear = y, keySeason = s, keyChar = c }

