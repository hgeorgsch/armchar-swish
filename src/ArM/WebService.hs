{-# LANGUAGE OverloadedStrings #-}

module ArM.WebService (stateScotty) where

import Web.Scotty  as S
import Network.HTTP.Types

import Control.Monad
import qualified Data.Text.Lazy as  T
import Swish.RDF.Formatter.Turtle (formatGraphAsText)
import qualified Swish.RDF.Graph as G
import Control.Monad.IO.Class (liftIO)

import qualified ArM.Character as C
import qualified ArM.CharacterQuery as CQ
import qualified ArM.CharacterMap as CM
import ArM.JSON

import qualified Control.Concurrent.STM as STM
import qualified ArM.Resources as AR
import ArM.JSON 

import Network.Wai.Middleware.RequestLogger ( logStdoutDev )

import System.CPUTime
import ArM.Time


jsonif' Nothing _  = notfound404
jsonif' (Just x) f =  jsonif'' x f

jsonif'' (CM.CharacterRecord x) f = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            json $ f x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

jsonif Nothing = notfound404
jsonif (Just x) = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            json x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"


stateScotty ::  G.RDFGraph -> G.RDFGraph -> G.RDFGraph -> STM.TVar CM.MapState -> S.ScottyM ()
stateScotty g schema res stateVar = do
        middleware logStdoutDev

        get "/" $ do     
          text "Test a get call - available paths for get:\n  /    (this page)\n  /graph\n  /initial\n  /gamestart\n  /res\n  /schema\n"
        get "/schema" $ do     
          text $ T.fromStrict $ formatGraphAsText $ schema
        get "/res" $ do     
          text $ T.fromStrict $ formatGraphAsText $ res
          liftIO $ print "FOOBAR"
        get "/graph" $ do     
          text $ T.fromStrict $ formatGraphAsText $ g
        get "/gamestart/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          jsonif $ C.getGameStartCharacter g char 
        get "/initial/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          jsonif $ C.getInitialCS g char 
        get "/test/adv/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          text $ T.pack $ show $ C.getIngameAdvancements g char
        get "/adv/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          jsonif $ Just $ C.getIngameAdvancements g char
        get "/pregameadvancement/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          jsonif $ Just $ C.getPregameAdvancements g char
        get "/cs/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                text $ T.fromStrict $ formatGraphAsText $ cgraph
             Nothing -> notfound404 
        get "/virtue/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getVirtues 
        get "/flaw/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getFlaws 
        get "/pt/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getPTs 
        get "/ability/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getAbilities 
        get "/characteristic/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getCharacteristics 

        S.delete "/" $ do
          html "This was a DELETE request!"  -- send 'text/html' response
        post "/" $ do
          text "This was a POST request!"
        put "/" $ do
          text "This was a PUT request!"
        put "/adv" $ do
          adv <- jsonData :: ActionM C.Advancement 
          liftIO $ print adv
          text $ T.pack $ show adv

notfound404 = do status notFound404
                 text "404 Not Found."

getCSGraph stateVar = do
          (char, year, season) <- getParam
          cmap <- liftIO $ CM.stMap <$> STM.readTVarIO stateVar
          let r = CM.lookup cmap char season (read year)
          return r

getParam = do
          char' <- param  "char"
          let char = "armchar:" ++ char'
          liftIO $ print $ "char: " ++ char
          year <- param  "year"
          liftIO $ print $ "year: " ++ year
          season <- param  "season"
          liftIO $ print $ "season: " ++ season
          return (char, year, season)
