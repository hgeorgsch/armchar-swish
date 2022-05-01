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
-- 1.  Transitive subclass/subproperty relations
-- 2.  Derive type properties
--
-----------------------------------------------------------------------------

module ArM.Rules.RDFS where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import ArM.Resources
import ArM.Rules.Aux

-- | RDFS Rules
rdfsRules = [
    makeRule "subclassRule" 
       "?s rdfs:subClassOf ?t . ?t rdfs:subClassOf ?c ."
       "?s rdfs:subClassOf ?c ."
    , makeRule "subpropRule" 
       "?s rdfs:subPropertyOf ?t . ?t rdfs:subPropertyOf ?c ."
       "?s rdfs:subPropertyOf ?c ."
       ]

-- | Rules to infer additional types and properties from subclass 
-- and subproperty relations (using RDFS vocabulary).
rdfstypeRules = [
    makeRule "subclasstypeRule"
       "?s rdf:type ?t . ?t rdfs:subClassOf ?c ."
       "?s rdf:type ?c ."
    , makeRule "subpropinstanceRule" 
       "?s ?p ?o . ?p rdfs:subPropertyOf ?p2 ."
       "?s ?p2 ?o ."
       ]

-- | Make inferences on the joint graph including resources
applyRDFS = fwdApplyList rdfstypeRules . fwdApplyListR rdfsRules 
