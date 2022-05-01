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
import ArM.Rules.InitialCharacter
import ArM.Rules.RDFS
import qualified ArM.Rules.Schema as RS

import qualified ArM.Rules.Resource as RR
import qualified ArM.Rules.FullGraph as RG

-- Initial Character Rules


-- | Initial inferences on the character data, to be applied without
-- the schema
prepareCS :: RDFGraph -> RDFGraph
prepareCS = fwdApplyList [ initialsheetRule, traitclasstypeRule ]

-- | Make all necessary inferences before retrieving character data
-- prepareInitialCharacter :: RDFGraph -> RDFGraph
-- prepareInitialCharacter = 
   -- fwdApplyList (
      -- csRule:advtypeRule:traitclassRule:advancementindexRule:rdfstypeRules )
prepareInitialCharacter = ArM.Rules.InitialCharacter.prepareInitialCharacter

-- | Apply standard RDFS rules to elaborate the schema
-- This is used only once, so it may be allowed to be costly.
prepareSchema :: RDFGraph -> RDFGraph
prepareSchema = fwdApplyListR rdfsRules

-- | Final inference to be done after merging with the resource graph
-- This is expensive, and may need caution.
-- It will be applied every time the graph changes, and the graph
-- is large
prepareGraph = RG.prepareGraph 

prepareResources = RR.prepareResources . applyRDFS
                 . fwdApplyList [ traitclasstypeRule ]

prepareRecord schema = RS.prepareCS schema 
                 . fwdApplyList [ traitclasstypeRule ]
