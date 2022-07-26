{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Load
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Functions to load the RDF graphs
--
-----------------------------------------------------------------------------

module ArM.Load where

import ArM.Resources
import ArM.Rules
import ArM.Rules.RDFS

import System.IO ( IO, readFile )
import Swish.RDF.Graph (emptyGraph,RDFGraph,merge)
import qualified Data.Text.Lazy.IO as DTLIO
import Swish.RDF.Parser.Turtle (parseTurtle)
import Control.Parallel

-- | Read an RDFGraph, ignoring errors.
readGraph :: String -> IO RDFGraph
readGraph fn = do
        contents <- DTLIO.readFile fn 
        case ( parseTurtle ( contents ) baseURI ) of
           (Left s ) -> do
               print s
               return emptyGraph
           (Right g ) -> return g

deriveGraph (c,s,r) = s1 `par` c0 `par` r0 `pseq` (c1,s1,r1)
     where  c1 = prepareGraph  $ merge r1 c0
            c0 = prepareCharGraph c
            s1 = prepareSchema s
            r1 = applyRDFS $ merge s1 r0
            r0 = prepareResources r

-- | Load the different graph and make initial inferences
-- See diagram in README.
-- Note: the third graph `res` has merged the schema and the resource
-- graph.  This is not reflected in the diagram.
getGraph :: String -> String -> String -> IO (RDFGraph,RDFGraph,RDFGraph)
getGraph characterFile armFile resourceFile = do
        c <- readGraph characterFile 
        s <- readGraph armFile 
        r <- readGraph resourceFile 
        return $ deriveGraph (c,s,r)

