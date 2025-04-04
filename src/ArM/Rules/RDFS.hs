{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.RDFS
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Rules and Rules Application for ArM Character Sheets. 
--
-----------------------------------------------------------------------------

module ArM.Rules.RDFS where

import Swish.RDF.Graph
import ArM.Rules.Aux
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Ruleset (RDFRule)

-- | Infer subclass and subproperty relations by transitivity
rdfsRules :: [RDFRule]
rdfsRules = [
  makeCRule "subclassRule" 
      [ arc sVar (Res rdfsSubClassOf) tVar 
      , arc tVar (Res rdfsSubClassOf) cVar ]
      [ arc sVar (Res rdfsSubClassOf) cVar ]
  , makeCRule "subpropRule" 
      [ arc sVar (Res rdfsSubPropertyOf) tVar 
      , arc tVar (Res rdfsSubPropertyOf) cVar ]
      [ arc sVar (Res rdfsSubPropertyOf) cVar ]
  ]

-- | Rules to infer additional types and properties from subclass 
-- and subproperty relations (using RDFS vocabulary).
rdfstypeRules :: [RDFRule]
rdfstypeRules = [
    makeCRule "subclasstypeRule"
      [ arc sVar typeRes tVar 
      , arc tVar (Res rdfsSubClassOf) cVar ]
      [ arc sVar typeRes cVar ]
    , makeCRule "subpropinstanceRule" 
      [ arc sVar pVar oVar 
      , arc pVar (Res rdfsSubPropertyOf) (Var "p2") ] 
      [ arc sVar (Var "p2") oVar ]
    ]

-- | Make inferences on the joint graph including resources
applyRDFS :: RDFGraph -> RDFGraph
applyRDFS = fwdApplyList rdfstypeRules . fwdApplyListR rdfsRules 
