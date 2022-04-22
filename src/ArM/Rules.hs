{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Rules and Rules Application for ArM Character Sheets. 
--
-----------------------------------------------------------------------------

module ArM.Rules 
  -- ( prepareInitialCharacter)
  where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.RDFS

import qualified ArM.Rules.Resource as RR

-- Initial Character Rules

-- | Infer character sheet properties from character properties
csRule = makeRule "csRule" 
    "?cs <https://hg.schaathun.net/armchar/schema#isCharacter> ?c . ?c ?p ?o ."
    "?cs ?p ?o ."

-- | Infer a string representation of the Advancement Type
-- (Reading, Practice, Exposure, etc.)
advtypeRule = makeRule "advtypeRule" 
    ( "?s <https://hg.schaathun.net/armchar/schema#hasAdvancementType> ?c ."
    ++ " ?c rdfs:label ?l ." )
    "?s <https://hg.schaathun.net/armchar/schema#hasAdvancementTypeString> ?l . "

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

-- | Initial inferences on the character data, to be applied without
-- the schema
prepareCS :: RDFGraph -> RDFGraph
prepareCS = fwdApplyList [ initialsheetRule, traitclasstypeRule ]

-- | Make all necessary inferences before retrieving character data
prepareInitialCharacter :: RDFGraph -> RDFGraph
prepareInitialCharacter = 
   fwdApplyList (
      csRule:advtypeRule:traitclassRule:advancementindexRule:rdfstypeRules )

-- | Apply standard RDFS rules to elaborate the schema
-- This is used only once, so it may be allowed to be costly.
prepareSchema :: RDFGraph -> RDFGraph
prepareSchema = fwdApplyListR rdfsRules

-- | Final inference to be done after merging with the resource graph
-- This is expensive, and may need caution.
-- It will be applied every time the graph changes, and the graph
-- is large
prepareGraph = RR.prepareGraph . applyRDFS
