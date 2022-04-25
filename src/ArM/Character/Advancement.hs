{-# LANGUAGE OverloadedStrings #-}

module ArM.Character.Advancement where

import Swish.RDF.Graph 
import Swish.RDF.Query as Q
import ArM.Resources 
import ArM.Query (qparse) 
import ArM.KeyPair 
import Data.Maybe 
import Data.List
import ArM.Character.Trait

-- Class:
--    a arm:CharacterAdvancement ;
-- Time:
--    arm:atSeasonTime arm:summer1217 ;
-- Character?
--    arm:advanceToCharacterSheet :autumn1217
-- Descriptive:
--    arm:hasAdvancementDescription "Studies Herbam L6 Q21 +3" ;
--    arm:awardsXP 21 ;
--    arm:hasAdvancementType arm:Reading ;
-- Traits (multiple)
--    arm:advanceTrait [ a armr:herbam ; arm:addedXP 21 ] ;

qt :: String -> RDFGraph
qt s = qparse $ prefixes 
      ++ s ++ " <https://hg.schaathun.net/armchar/schema#advanceTrait> ?id . " 
      ++ "?id ?property ?value . "
      ++ "?property rdfs:label ?label . "


-- | Get a list of all Pregame Advancements of a character.
getPregameAdvancements :: RDFGraph -> String -> [Advancement]
getPregameAdvancements g c = getAdvancements g $ qparse $ prefixes 
       ++ " ?id rdf:type <https://hg.schaathun.net/armchar/schema#PregameAdvancement> ;"
       ++ " ?property ?value ; "
       ++ " <https://hg.schaathun.net/armchar/schema#advanceCharacter> "
       ++ c ++ " . "
       ++ "?property rdfs:label ?label . "
-- | Get a list of all Ingame Advancements of a character.
getIngameAdvancements :: RDFGraph -> String -> [Advancement]
getIngameAdvancements g c = getAdvancements g $ qparse $ prefixes 
       ++ " ?id rdf:type arm:IngameAdvancement ; "
       ++ " ?property ?value ; "
       ++ " <https://hg.schaathun.net/armchar/schema#advanceCharacter> "
       ++ c ++ " . "
       ++ "?property rdfs:label ?label . "

-- | Generic version of 'getIngameAdvancements' and 'getPregameAdvancements'
getAdvancements :: RDFGraph -> RDFGraph -> [Advancement]
getAdvancements g = fixAdvancements g . 
               map toAdvancement . keypairSplit . getGenQuads g 

-- | Auxiliary for 'getAdvancements'
getGenQuads :: RDFGraph -> RDFGraph -> [ObjectKeyValue]
getGenQuads g q = map objectFromBinding $ rdfQueryFind q g

-- | Auxiliary for 'getAdvancements'
fixAdvancements :: RDFGraph -> [Advancement] -> [Advancement]
fixAdvancements g adv = map (fixAdv g) adv

-- | Auxiliary for 'fixAdvancements'
fixAdv :: RDFGraph -> Advancement -> Advancement
fixAdv g a = a { traits = sort $ getTraits q g }
    where qt' = qt . show . fromJust . rdfid 
          q = qt' a
          getTraits q g = map toTrait
               $ keypairSplit $ map objectFromBinding $ rdfQueryFind q g 

advancementIDstring = show . fromJust . rdfid 

-- | CharacterAdvancement Resource
-- Essential information is in `rdfid`, `contents`, and `traits.
-- The other properties are redundant, merely giving access to
-- critical information without having to search the lists.
-- TraitAdvancements are represented as a list of `Trait`s.
-- Other properties are listed as 'contents'.
data Advancement = Advancement {
    year :: Maybe Int,
    season :: Maybe RDFLabel,
    rdfid :: Maybe RDFLabel,
    contents :: [KeyValuePair],
    advSortIndex :: Int,
    traits :: [Trait]
   } deriving Eq

defaultAdvancement = Advancement { year = Nothing,
                season = Nothing, rdfid = Nothing, 
                advSortIndex = 0,
                contents = [], traits = [] }
instance Show Advancement where
   show a = "**" ++ y (season a) ++ " " ++ y (year a) ++ "**\n" 
                 ++ sc (contents a) 
                 ++ show (traits a) 
                 ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         sc [] = ""
         sc (KeyValuePair x y:xs) = show x ++ ": " ++ show y ++ "\n" ++ sc xs
         st [] = ""
         st ((x,_,y,z):xs) = "  " ++ show x ++ ": " ++ y ++ " - " ++ z 
                                  ++  "\n" ++ st xs
instance Ord Advancement where
   compare x y | advSortIndex x < advSortIndex y = LT
               | advSortIndex x > advSortIndex y = GT
               | year x < year y = LT
               | year x > year y = GT
               | sno x < sno y = LT
               | sno x > sno y = GT
               | rdfid x < rdfid y = LT
               | rdfid x > rdfid y = GT
               | contents x < contents y = LT
               | contents x > contents y = GT
               | otherwise = EQ
               where sno = seasonNo . season

seasonNo Nothing = 0
seasonNo (Just x ) | x == springLabel = 1
                   | x == summerLabel = 2
                   | x == autumnLabel = 3
                   | x == winterLabel = 4
                   | otherwise  = 10

-- | Make an Advancement object from a list of Quads
toAdvancement :: [ObjectKeyValue] -> Advancement
toAdvancement [] = defaultAdvancement 
toAdvancement xs = defaultAdvancement { rdfid = Just $ okvSubj $ head xs,
         year = getYear ys,
         season = getSeason ys,
         advSortIndex = getSortIndex ys,
         contents = ys }
         where ys = toKeyPairList xs 

-- | Get the year from a list of Triples belonging to an Advancement
getYear :: [KeyValuePair] -> Maybe Int
getYear xs = f1 x
     where  x = getProperty inYear xs
            f1 Nothing = Nothing
            f1 (Just year) = fromRDFLabel year

-- | Get the season from a list of Triples belonging to an Advancement
getSeason = getProperty inSeason

-- | Get sort index from a list of Triples belonging to an Advancement
getSortIndex :: [KeyValuePair] -> Int
getSortIndex xs = f1 x
     where  x = getProperty hasAdvancementIndex xs
            f1 Nothing = 2^30
            f1 (Just idx) = f2 $ fromRDFLabel idx
            f2 Nothing = 2^30
            f2 (Just idx) = idx
