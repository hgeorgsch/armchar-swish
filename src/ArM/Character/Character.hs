{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- The CharacterSheet data type and corresponding functions
--
-----------------------------------------------------------------------------
module ArM.Character.Character where

import Data.Set (fromList)
import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import qualified Data.Text.Lazy as T
import Swish.RDF.VarBinding as VB 
import Network.URI (URI,parseURI)
import Swish.VarBinding  (vbMap)
import Data.Maybe
import Data.List (sort)
import ArM.Resources
import ArM.Character.Trait
import ArM.Character.Advancement
import ArM.KeyPair
import qualified ArM.Character.Metadata as CM
import ArM.BlankNode
import ArM.Rules.Aux

getCharacterMetadata = CM.getCharacterMetadata

data CharacterSheet = CharacterSheet {
         csID :: String,
         sheetID :: Maybe RDFLabel,
         csYear :: Maybe Int,
         csSeason :: String,
         -- csSeason :: Maybe RDFLabel,
         csTraits :: [Trait],
         csMetadata :: [KeyValuePair]
       }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ csID cs ++ "**\n" 
           ++ "-- " ++ ( show . fromJust . sheetID ) cs ++ "\n"
           ++ "Traits:\n" ++ showw ( csTraits cs )
           ++ "Metadata Triples:\n" ++ showw ( csMetadata cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCS = CharacterSheet {
         csID = "",
         sheetID = Nothing,
         csYear = Nothing,
         csSeason = "",
         csTraits = [],
         csMetadata = []
       }  

getGameStartCharacter :: RDFGraph -> String -> CharacterSheet
getGameStartCharacter g = getGameStartCS g . getInitialCS g

getGameStartCS :: RDFGraph -> CharacterSheet -> CharacterSheet
getGameStartCS g cs = foldl advanceCharacter cs as
    where as = sort $ getPregameAdvancements g $ csID cs

-- | Given a graph and a string identifying a character
-- make a list of all ingame character sheets for the 
-- character by applying all available advancements.
getAllCS :: RDFGraph -> String -> [CharacterSheet]
getAllCS g c = cs:advanceList cs as
    where cs = getGameStartCharacter g c
          as = sort $ getIngameAdvancements g c

-- | Given a character sheet and a sorted list of advancements,
-- apply all the advancements in order and produce a list
-- of character sheets for every step
advanceList :: CharacterSheet -> [Advancement] -> [CharacterSheet]
advanceList _ [] = []
advanceList cs (x:xs) = advanceCharacter cs x : advanceList cs xs

-- | apply a given Advancement to a given CharacterSheet
advanceCharacter :: CharacterSheet -> Advancement -> CharacterSheet 
advanceCharacter cs adv = cs { 
     sheetID = Nothing,
     csYear = year adv,
     csSeason = season adv,
     csTraits = advanceTraitList (csTraits cs) (traits adv)
     }


-- | get initial CharacterSheet from an RDFGraph
getInitialCS :: RDFGraph -> String -> CharacterSheet
getInitialCS g = fixCS g . getInitialCS' g

getInitialCS' :: RDFGraph -> String -> CharacterSheet
getInitialCS' g c = defaultCS {
            csID = c,
            sheetID = x,
            csTraits = [],
            csMetadata = getCharacterMetadata g cs
         }
         where cs = show $ fromJust $ x
               x = getInitialSheet g c
     

-- | Given an identifier for the character, find the identifier for
-- the initial character sheet
getInitialSheet :: RDFGraph -> String -> Maybe RDFLabel
getInitialSheet g c = vbMap vb (G.Var "s")
    where 
      vb = head $ rdfQueryFind q g
      q = qparse $ prefixes ++ c
        ++ " <https://hg.schaathun.net/armchar/schema#hasInitialSheet> ?s . " 


-- | Auxiliary for 'getInitialSheet'
fixCS :: RDFGraph -> CharacterSheet -> CharacterSheet
fixCS g a = a { csTraits = sort $ getTraits a g }

-- | Get a list of traits. Auxiliary function for 'fixCS'.
getTraits :: CharacterSheet -> RDFGraph -> [Trait]
getTraits a g = map toTrait 
               $ keypairSplit $ map objectFromBinding $ rdfQueryFind q g 
    where q = cqt $ show $ fromJust $ sheetID a

-- | Query Graph to get traits for CharacterSheet
cqt :: String -> RDFGraph
cqt s = qparse $ prefixes 
      ++ s ++ " <https://hg.schaathun.net/armchar/schema#hasTrait> ?id . " 
      ++ "?id ?property ?value . "
      ++ "?property rdfs:label ?label . "

csToRDFGraph :: CharacterSheet -> RDFGraph
csToRDFGraph cs =
         ( toRDFGraph .  fromList . fst . runBlank ( csToArcListM cs ) )
         ("charsheet",1)

csToArcListM :: CharacterSheet -> BlankState [RDFTriple]
csToArcListM cs = do
          x <- getSheetIDM cs $ sheetID cs
          ts <- mapM (traitToArcListM x) (csTraits cs)
          let ms = triplesToArcList x (csMetadata cs)
          let ct = arc x isCharacterLabel charlabel
          let ct1 = arc x typeRes csRes 
          return $ ct1:ct:foldl (++) ms ts
    where 
          charlabel = getCSID cs

getCSID :: CharacterSheet -> RDFLabel
getCSID = toRDFLabel . fromJust . parseURI . csID 

getSheetIDM :: CharacterSheet -> Maybe RDFLabel -> BlankState RDFLabel
getSheetIDM _ Nothing = getBlank
getSheetIDM _ (Just x) = return x
