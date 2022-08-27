{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle Characters, Traits, and Advancements, with some basic
-- associated functions.
--
-----------------------------------------------------------------------------
module ArM.Types.Character where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import ArM.Types.Season
import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux


-- |
-- = RDF Conversion

class FromRDFGraph a where
    fromRDFGraph :: RDFGraph -> RDFLabel -> a 
class ToRDFGraph a where
    makeRDFGraph :: a -> RDFGraph

-- | Convert `KeyValuePair` to `RDFTriple`
-- This is an auxiliary for other ToRDFGraph functions
keyvalueToArcList :: RDFLabel -> [KeyValuePair] -> [RDFTriple]
keyvalueToArcList x [] = []
keyvalueToArcList x (KeyValuePair a c:ys) = arc x a c:keyvalueToArcList x ys

-- | 
-- = Trait

-- | Trait Resource
-- `traitID` and `traitContents` are sufficient to describe the trait.
-- The other fields duplicate information to facilitate searching and
-- sorting.
-- When new traits are created, `traitID` is set to nothing?
-- A blank node is only created when it is written into an RDFGraph.
data Trait = Trait {
    traitClass :: RDFLabel,
    instanceLabel :: String,
    isRepeatableTrait :: Bool,
    traitContents :: [RDFTriple]
   } deriving (Eq)
defaultTrait = Trait {
    traitClass = noSuchTrait,
    instanceLabel = "",
    isRepeatableTrait = False,
    traitContents = []
   } 

-- | Get the ID (RDFLabel) of a trait if possible.
traitID :: Trait -> Maybe RDFLabel
traitID = f . traitContents
   where f [] = Nothing
         f (x:_) = Just $ arcSubj x

instance Show Trait where
   show a = "**" ++ show (traitClass a) ++ "**\n" 
                 ++ sc (traitContents a) 
                 ++ "\n"
      where 
         sc [] = ""
         sc (x:xs) = "  " ++ show x ++ "\n" ++ sc xs
instance Ord Trait where
   compare x y | traitClass x < traitClass y = LT
               | traitClass x > traitClass y = GT
               -- | not (isRepeatableTrait x) = EQ
               -- | not (isRepeatableTrait y) = EQ
               | instanceLabel x < instanceLabel y = LT
               | instanceLabel x > instanceLabel y = GT
               | otherwise = EQ

-- | 
-- = Character


data Character = Character {
         characterID :: RDFLabel,
         characterData :: KeyPairList
       }  deriving (Eq)
instance Show Character where
    show cs = "**" ++ show (characterID cs) ++ "**\n" 
           ++ "Metadata Triples:\n" ++ show ( characterData cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCharacter = Character {
         characterID = noSuchCharacter,
         characterData = KeyPairList []
       }  

-- | 
-- = Character Sheet

data CharacterSheet = CharacterSheet {
      csID :: RDFLabel,
      -- ^ Character ID (i.e. same ID for every season)
      sheetID :: Maybe RDFLabel,  
      -- ^ ID of the Character Sheet, usually Nothing suggesting a blank node
      csTime :: CharTime,  -- ^ Current Year
      born     :: Int,      -- ^ Year of Birth
      csItems :: [Trait],    -- ^ List of possessions (weapons, equipment)
      csTraits :: [Trait],  -- ^ List of traits (abilities, spells, etc.)
      csMetadata :: KeyPairList
      -- ^ Metadata, i.e. data which are not traits or items.
      }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ show (csID cs) ++ "**\n" 
           ++ "-- " ++ ( showSheetID ) cs ++ "\n"
           ++ "Traits:\n" ++ showw ( csTraits cs )
           ++ "Metadata Triples:\n" ++ show ( csMetadata cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCS = CharacterSheet {
         csID = noSuchCharacter,
         sheetID = Nothing,
         csTime = defaultCharTime,
         born = 0,
         csItems = [],
         csTraits = [],
         csMetadata = KeyPairList []
       }  

showSheetID :: CharacterSheet -> String
showSheetID = f . sheetID
    where f Nothing = "no sheet ID"
          f (Just x) = show x


instance ToRDFGraph CharacterSheet where
   makeRDFGraph cs =
         ( listToRDFGraph  . fst . runBlank ( csToArcListM cs' ) )
         ("charsheet",1)
      where cs' = cs { csMetadata = KeyPairList $ a xs }
            KeyPairList xs = csMetadata cs
            aAge age ys  
                   | age == 0 = ys
                   | otherwise = KeyValuePair (armRes "hasAge") (litInt age):ys
            aYear Nothing ys   = ys
            aYear (Just x) ys  = KeyValuePair (armRes "inYear") (litInt x):ys
            aSeason x ys  
                   | x == "" = ys
                   | otherwise = KeyValuePair (armRes "atSeason") (litString x):ys
            age' Nothing _ = 0
            age' (Just y) b | b == 0 = 0 
                            | otherwise = y - b
            age = age' (hasYear cs) (born cs)
            a = aAge age . aYear (hasYear cs) . aSeason (hasSeason cs)
instance ToRDFGraph Character where
   makeRDFGraph cs = listToRDFGraph  ( ct:ms )
       where x = characterID cs
             ms = keyvalueToArcList x (fromKeyPairList $ characterData cs)
             ct = arc x typeRes (armRes  "Character")

csToArcListM :: CharacterSheet -> BlankState [RDFTriple]
csToArcListM cs = do
          x <- getSheetIDM cs $ sheetID cs
          tsm <- fixBlanksM $ csTraits cs
          ism <- fixBlanksM $ csItems cs
          let ht = map ( \ y -> arc x (armRes "hasTrait") (fromJust $ traitID y) ) tsm
          let hi = map ( \ y -> arc x (armRes "hasPossession") (fromJust $ traitID y) ) ism
          let ts =  map traitContents tsm
          let is =  map traitContents ism
          let metadata = keyvalueToArcList x (fromKeyPairList $ csMetadata cs)
          let ms = metadata ++ hi ++ ht 
          let ct = arc x isCharacterLabel (csID cs)
          let ct1 = arc x typeRes csRes 
          let ms1 = foldr (++) ms ts
          return $ ct1:ct:foldr (++) ms1 is

fixBlanksM :: [Trait] -> BlankState [Trait]
fixBlanksM [] = return []
fixBlanksM (x:xs) = do
             x' <- fixBlankNodeM x
             xs' <- fixBlanksM xs
             return $ x':xs'
fixBlankNodeM :: Trait -> BlankState Trait
fixBlankNodeM t 
   | traitContents t == [] = return t
   | key /= (armRes "unnamedBlankNode") = return t
   | otherwise = do
        b <- getBlank
        return $ t { traitContents = map ( replaceBlank b ) 
                      $ traitContents t }
     where key = arcSubj $ head $ traitContents t

replaceBlank :: RDFLabel -> RDFTriple -> RDFTriple
replaceBlank b x = arc b ( arcPred x ) ( arcObj x )
            
getSheetIDM :: CharacterSheet -> Maybe RDFLabel -> BlankState RDFLabel
getSheetIDM _ Nothing = getBlank
getSheetIDM _ (Just x) = return x

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

instance HasTime CharacterSheet where
    timeOf = csTime
