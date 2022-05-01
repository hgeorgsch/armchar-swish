-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Schema
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules using the schema ontology.
-- 1. Copy labels
-- 2. Subproperties of arm:hasTrait
--
-----------------------------------------------------------------------------

module ArM.Rules.Schema where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.XSD
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.RDFS
import Swish.VarBinding (varBindingId) 

import Control.Parallel.Strategies


-- prepareCS schema g = foldl addGraphs g $ fwdApplyMap rs gg
                 -- where gg = merge schema g
                       -- rs = copyRules  ++ traitRules  ++ rdfstypeRules
prepareCS schema = fwdApplyListS schema copyRules 
                 . fwdApplyListS schema traitRules 
                 . fwdApplyListS schema rdfstypeRules

copyRules = [ makeCRule "cp1" [arc pVar labelRes oVar]
                               [arc pVar labelRes oVar]
             ]
traitRules = traitRules1 ++ traitRules2
traitRules1 = map mkr tcs
    where mkr s = mkr' ("has" ++ s ++ "Rule")
                       (Res $ makeSN s) (Res $ makeSN $ "has" ++ s)
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs1 t p
traitRules2 = map mkr tcs
    where mkr s = mkr' ("has" ++ s ++ "IRule")
                       (Res $ makeSN s) (Res $ makeSN $ "has" ++ s)
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs2 t p
tcs = [ "Ability"
      , "Virtue"
      , "Flaw"
      , "PersonalityTrait"
      , "Reputation"
      , "Spell"
      , "Art"
      , "OtherTrait"
      , "Characteristic" ]

arcs1 t p = ( [ arc cVar htRes tVar, arc tVar typeRes t ],
             [ arc cVar p tVar ] ) 
arcs2 t p = ( [ arc cVar p tVar, arc tVar typeRes t ],
             [ arc cVar htRes tVar ] ) 
