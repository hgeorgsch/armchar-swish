-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Resources
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules using the resource ontology.
--
-----------------------------------------------------------------------------

module ArM.Rules.Resource (prepareResources) where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.XSD
import ArM.Resources
import ArM.Rules.Aux
-- import ArM.Rules.RDFS
import Swish.VarBinding (varBindingId) 

prepareResources = fwdApplyList [ vfpRule, vfpMajorRule, vfabRule ] 
                 . fwdApplyListR [ vfvRule, vffRule ]

traitclass = Var "traitclass"
grants = gtRes
trait = Var "trait"
score = Res $ makeSN "hasScore"

grantarc = arc traitclass grants trait

vfpRule = makeCRule "vfpRule" [grantarc, vfp2, vfp3] [vfpT]
vfpMajorRule = makeCRule "vfpMajorRule" [grantarc, vfp2bis, vfp3] [vfpTbis]
vfp2 = arc traitclass subclassRes (Res $ makeSN "minorFlaw")
vfp2bis = arc traitclass subclassRes (Res $ makeSN "majorFlaw")
vfp3 = arc trait typeRes (Res $ makeSN "PersonalityTrait" )
vfpT = arc trait score (litInt 3)
vfpTbis = arc trait score (litInt 6)

litInt i = TypedLit (T.pack $ show i) xsdInteger

vfabRule = makeCRule "vfabRule" [grantarc, vfab3] [vfabT]
vfab3 = arc trait typeRes (Res $ makeSN "Ability" )
vfabT = arc trait (Res $ makeSN "hasTotalXP") (litInt 5)

vfvRule = makeCRule "vfvRule" [grantarc, vfv3] [vfT]
vffRule = makeCRule "vffRule" [grantarc, vff3] [vfT]
vfv3 = arc trait typeRes (Res $ makeSN "Virtue" )
vff3 = arc trait typeRes (Res $ makeSN "Flaw" )
vfT = arc trait score (litInt 0)
