{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Char.CharacterSheet ( CharacterSheet(..)
                               , characterSheet
                               , filterCS
                               ) where

import ArM.Char.Trait
import ArM.Char.Internal.Character
import ArM.Char.Internal.Advancement
import GHC.Generics
import Data.Aeson
import Data.Maybe

import ArM.Debug.Trace

data CharacterSheet = CharacterSheet 
         { csTime :: CharTime
         , csType :: CharacterType
         , vfList :: [ VF ]
         , abilityList :: [ Ability ]
         , artList :: [ Art ]
         , spellList :: [ Spell ]
         , reputationList :: [ Reputation ]
         , csTraits :: [ Trait ]
         }  deriving (Eq,Show,Generic)

defaultSheet :: CharacterSheet 
defaultSheet = CharacterSheet 
         { csTime = Nothing
         , csType = Magus
         , vfList = [ ]
         , abilityList = [ ]
         , artList = [ ]
         , spellList = [ ]
         , reputationList = []
         , csTraits = [ ]
         }  

characterSheet :: Character -> CharacterSheet
characterSheet c | isNothing st = defaultSheet { csType = charType (concept c) }
                 | otherwise = cs  { csType = charType (concept c) }
    where st = state c
          cs = filterCS $ fromJust st 

instance ToJSON CharacterSheet where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterSheet 

filterCS :: CharacterState -> CharacterSheet
filterCS cs = trace ("filterCS "++show cs) $ defaultSheet  { csTime = charTime cs
                 , vfList = trace ("x1=" ++ show x1) x1
                 , abilityList = x2
                 , artList = x3
                 , spellList = x4
                 , reputationList = x5
                 , csTraits = y5
                }
           where (x1,y1) = filterTrait $ traits cs
                 (x2,y2) = filterTrait y1
                 (x3,y3) = filterTrait y2
                 (x4,y4) = filterTrait y3
                 (x5,y5) = filterTrait y4
