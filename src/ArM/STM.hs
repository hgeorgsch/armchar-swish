module ArM.STM where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe (fromJust)
import Network.URI (URI)

import qualified ArM.CharacterMap as CM
import qualified ArM.Character as C
import qualified ArM.Types.Character as TC
import qualified ArM.Resources as AR

import ArM.Rules.Aux
import ArM.Rules.RDFS
import qualified ArM.Rules as R
import ArM.Resources
import Swish.RDF.Graph

import Data.Set (fromList)



data MapState = MapState { graph :: G.RDFGraph,
                           schemaGraph :: G.RDFGraph,
                           resourceGraph :: G.RDFGraph,
                           resGraph :: G.RDFGraph }

getSTM r s g char = STM.newTVarIO MapState { 
                  graph = g',
                  schemaGraph = schema,
                  resourceGraph = res,
                  resGraph = res'  }
                  where res' = R.prepareResources $ res `merge` schema
                        g' = R.prepareGraph res g
                        schema = R.prepareSchema s
                        res = R.prepareResources r

getStateGraph :: STM.TVar MapState -> IO G.RDFGraph
getStateGraph st = fmap graph $ STM.readTVarIO st
getSchemaGraph :: STM.TVar MapState -> IO G.RDFGraph
getSchemaGraph st = fmap schemaGraph $ STM.readTVarIO st
getResourceGraph :: STM.TVar MapState -> IO G.RDFGraph
getResourceGraph st = fmap resourceGraph $ STM.readTVarIO st
getResGraph :: STM.TVar MapState -> IO G.RDFGraph
getResGraph st = fmap resGraph $ STM.readTVarIO st
getCGraph st = do
     g <- getStateGraph
     res <- getResGraph
     return $ g `merge` res

persistGraph schema g = foldGraphs $ Q.rdfQuerySubs vb tg
    where vb = Q.rdfQueryFind qg g'
          qg = G.toRDFGraph $ fromList [ arc sVar pVar cVar,
                       arc pVar typeRes armPersistentProperty ]
          tg = G.toRDFGraph $ fromList [ arc sVar pVar cVar ]
          g' = rdfs $ schema `merge` g

persistRule = makeCRule "persistRule" 
    [ arc sVar pVar cVar,
      arc pVar typeRes armPersistentProperty ]
    [ arc sVar pVar cVar ]
-- persistGraph' schema = fwdApplySimple persistRule . rdfs schema 

rdfs g = merge g $ fwdApplyList rdfstypeRules g 

lookup :: STM.TVar MapState -> String -> String -> Int 
       -> IO (Maybe CM.CharacterRecord)
lookup stateVar char season year = do
          g <- getStateGraph stateVar
          res <- getResGraph stateVar
          print $ char ++ " - " ++ season ++ " - " ++ show year
          let cl =  C.getAllCS g $ AR.armcharRes char
          print $  AR.armcharRes char
          let charstring = "armchar:" ++ char
          case (cl) of
             Nothing -> return Nothing
             (Just x) -> return $ CM.lookup cmap charstring season year
                where  cmap = CM.insertListS res CM.empty $ x

-- getResource :: G.RDFGraph -> G.RDFLabel -> Maybe G.RDFGraph
-- getResource g label = Nothing


putGraph :: G.RDFGraph -> G.RDFGraph -> G.RDFGraph -> G.RDFGraph
putGraph g g0 g1 = G.merge (G.delete g0 g) g1

-- | Update the state graph with the given Advancement object.
putAdvancement :: STM.TVar MapState -> TC.Advancement -> IO G.RDFGraph
putAdvancement stateVar adv = do 
         STM.atomically $ do
             st <- STM.readTVar stateVar
             let g = graph st
             let res = resGraph st
             let schema = schemaGraph st
             let g1 = persistGraph schema $ TC.makeRDFGraph adv
             let adv0 = TC.fromRDFGraph g (TC.rdfid adv) :: TC.Advancement
             let g0 = TC.makeRDFGraph adv0
             let gg = putGraph g g0 g1
             st <- STM.readTVar stateVar
             STM.writeTVar stateVar $ st { graph = R.prepareGraph res gg }
             return g1
          
-- TODO: Check for conflicting advancements 


-- postResource :: STM.TVar MapState -> G.RDFLabel -> G.RDFGraph
             -- -> IO (Maybe G.RDFGraph)
-- postResource stateVar label newres = do 
      -- STM.atomically $ do
          -- st <- STM.readTVar stateVar
          -- let g = graph st
          -- case (putGraph g label newres) of
             -- Nothing -> return Nothing
             -- (Just gg) -> do
                          -- STM.writeTVar stateVar $ st { graph = gg }
                          -- return $ Just gg
