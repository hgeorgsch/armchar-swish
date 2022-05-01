{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.InitialCharacter
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Rules for the Initial Character Sheet
-- 1.  Expand arm:traitClass
-- 2.  Add some string representations
-----------------------------------------------------------------------------
module ArM.Rules.InitialCharacter where

import Swish.Rule
import Swish.RDF.Graph
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.RDFS

-- | Infer character sheet properties from character properties
csRule = makeRule "csRule" 
    "?cs <https://hg.schaathun.net/armchar/schema#isCharacter> ?c . ?c ?p ?o ."
    "?cs ?p ?o ."

-- | Infer a string representation of the Advancement Type
-- (Reading, Practice, Exposure, etc.)
advtypeRule = makeCRule "advtypeRule" 
    [ arc sVar hasAdvancementType cVar, arc cVar labelRes lVar ]
    [ arc sVar hasAdvancementTypeString lVar ]

-- | Infer a string representation of the Trait Class of each Trait Advancement
traitclassRule = makeCRule "traitclassRule" 
    [ arc sVar (Res $ makeSN "traitClass") cVar,
      arc cVar (Res $ makeSN "hasLabel") oVar ]
    [ arc sVar (Res $ makeSN "traitClassString") oVar ]
traitclasstypeRule = makeRule "traitclasstypeRule" 
       "?s <https://hg.schaathun.net/armchar/schema#traitClass> ?t . "
       "?s rdf:type ?t . "

-- | Add indices used for sorting advancements
advancementindexRule = makeCRule "advancementindexRule" 
    [ tArc, arc tVar (Res $ makeSN "hasAdvancementIndex") cVar ]
    [ arc sVar (Res $ makeSN "hasAdvancementIndex") cVar ]

-- | Add indices used for sorting advancements
initialsheetRule = makeCRule "initialsheetRule" 
    [ arc cVar  (Res $ makeSN "hasInitialSheet") sVar ]
    [ arc sVar (Res $ makeSN "isCharacter") cVar,
      arc sVar typeRes (Res $ makeSN "CharacterSheet") ]

initialCSrules = [ csRule
                 , advtypeRule
                 , traitclassRule
                 , traitclasstypeRule
                 , advancementindexRule
                 , initialsheetRule ]
prepareInitialCharacter schema = fwdApplyListS schema initialCSrules
                               . fwdApplyListS schema rdfstypeRules

