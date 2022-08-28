{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle Advancement
--
-----------------------------------------------------------------------------
module ArM.Types.Advancement where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import ArM.Types.Season
import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux
import ArM.Types.RDF
import ArM.Types.Trait
import qualified ArM.Types.Season as TS
import Data.Aeson
import Data.Aeson.Key

-- |
-- = Character Advancement

-- | CharacterAdvancement Resource
-- Essential information is in `rdfid`, `contents`, and `traits.
-- The other properties are redundant, merely giving access to
-- critical information without having to search the lists.
-- TraitAdvancements are represented as a list of `Trait`s.
-- Other properties are listed as 'contents'.
data Advancement = Advancement 
    { advChar :: RDFLabel
    , advTime :: CharTime
    , rdfid :: RDFLabel
    , contents :: [KeyValuePair]
    , traits :: [Trait]
    , items :: [Trait]
   } deriving Eq

advSortIndex :: Advancement -> Int
advSortIndex = advancementIndex . advTime
year :: Advancement -> Int
year = f . hasYear 
   where f Nothing = 0
         f (Just y) = y
season :: Advancement -> String
season = charSeason . advTime

defaultAdvancement = Advancement 
                { advChar = armRes "noSuchCharacter"
                , rdfid = noSuchAdvancement
                , contents = []
                , advTime = defaultCharTime
                , traits = []
                , items = []
                }
instance Show Advancement where
   show a = show (rdfid a) ++ "\n  **" ++ (season a) ++ " " ++ show (year a) ++ "**\n" 
                 ++ sc (contents a) 
                 ++ show (traits a) 
                 ++ show (items a) 
                 ++ "\nSort Index: " ++ show (advSortIndex a) 
                 ++ "\nSeason No: " ++ show (sno a) 
                 ++ "\n"
      where 
         sc [] = ""
         sc (KeyValuePair x y:xs) = show x ++ ": " ++ show y ++ "\n" ++ sc xs
         st [] = ""
         st ((x,_,y,z):xs) = "  " ++ show x ++ ": " ++ y ++ " - " ++ z 
                                  ++  "\n" ++ st xs

instance HasTime Advancement where
    timeOf = advTime
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

sno = seasonNo . season

instance ToRDFGraph Advancement where
   makeRDFGraph cs =  listToRDFGraph  ( advToArcList cs ) 

advToArcList :: Advancement -> [RDFTriple]
advToArcList adv = ys2
    where ms = keyvalueToArcList (rdfid adv) (contents adv)
          atRes = armRes "advanceTrait"
          cpRes = armRes "changePossession"
          xs1 =  map traitContents (traits adv)
          xs2 =  map traitContents (items adv)
          ys1 = foldr (++) ms xs1
          ys2 = foldr (++) ys1 xs2


-- |
-- = JSON

data ProtoAdvancement = ProtoAdvancement {
    advancementchar :: RDFLabel,
    advancementid :: RDFLabel,
    advancementcontents :: KeyPairList,
    advancementtraits :: [Trait],
    advancementitems :: [Trait]
   } 

instance ToJSON Advancement where 
    toJSON cs = object (c:s:x:z:y:[])
       where x = (fromString "advancementtraits") .= (toJSON (traits cs))
             z = (fromString "advancementitems") .= (toJSON (items cs))
             y = (fromString "advancementcontents") .= KeyPairList (contents cs)
             c = (fromString "advancementid") .= toJSON (rdfid cs)
             s = (fromString "advancementcharacter") .= toJSON (advChar cs)

instance FromJSON Advancement where 
   parseJSON = fmap fromProtoAdvancement . parseJSON
instance FromJSON ProtoAdvancement where 
   parseJSON (Object v) = ProtoAdvancement <$> v .: "advancementid"
                                           <*> v .: "advancementcontents"
                                           <*> v .: "advancementtraits"
                                           <*> v .: "advancementitems"
                                           <*> v .: "advancementcharacter"
fromProtoAdvancement :: ProtoAdvancement -> Advancement
fromProtoAdvancement adv = defaultAdvancement 
                     { rdfid = advancementid adv
                     , traits = advancementtraits adv
                     , items = advancementitems adv
                     , advChar = advancementchar adv
                     , advTime = parseTime TS.defaultCharTime ys
                     , contents = ys
                 } where ys = fromKeyPairList $ advancementcontents adv

parseTime :: CharTime  -> [KeyValuePair] -> CharTime
parseTime a [] = a
parseTime a (x:xs) = parseTime (f a x) xs
  where f a (KeyValuePair k v) 
         | k == inYear = a { charYear = rdfToInt v }
         | k == atSeason = a { charSeason = fs (rdfToString v) }
         | k == hasAdvancementIndex = a { advancementIndex = fi (rdfToInt v) }
         | otherwise = a
         where fs Nothing = "" 
               fs (Just x) = x
               fi Nothing = 0 
               fi (Just x) = x

instance FromRDFGraph Advancement where
   fromRDFGraph g label = fixAdv g $ defaultAdvancement 
                 { rdfid = label
                 , advTime = parseTime defaultCharTime ys 
                 , contents = ys }
        where q = listToRDFGraph  $
                  [ arc label typeRes (armRes "CharacterAdvancement"),
                    arc label (Var "property") (Var "value"),
                    arc (Var "property") typeRes armViewProperty,
                    arc (Var "property") labelRes (Var "label") ]
              vb = Q.rdfQueryFind g q
              ys = map keypairFromBinding vb

-- | Auxiliary for 'fixAdvancements'
fixAdv :: RDFGraph -> Advancement -> Advancement
fixAdv g adv = trace ("fixAdv "++show advid) $ adv { traits = traitsFromRDF advid g,
                 items = itemsFromRDF advid g }
        where advid = rdfid adv