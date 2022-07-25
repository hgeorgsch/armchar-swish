{-# LANGUAGE OverloadedStrings #-}
-- (C) 2022: Hans Georg Schaathun <hg+gamer@schaathun.net>

module Main where

import System.IO as IO
import Control.Monad (join)
import qualified Swish.RDF.Formatter.Turtle as TTL
import Swish.RDF.Graph (merge)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
--
-- import qualified Data.Text.Lazy as  T
-- import Data.Text (Text)

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B

-- import qualified Control.Concurrent.STM as STM
import ArM.STM

-- Web service
import qualified Web.Scotty  as S
import ArM.WebService (stateScotty)

-- Auth
-- import qualified Network.Wai.Middleware.HttpAuth as HA
import Data.SecureMem -- for constant-time comparison

-- Timer
import ArM.Time

import ArM.Load (getGraph)
import qualified ArM.Resources as AR
import qualified ArM.Character.Character as C
import qualified ArM.CharacterQuery as CQ
import qualified ArM.CharacterMap as CM


authf u p = return $ u == "user" && secureMemFromByteString p == password
password :: SecureMem
password = secureMemFromByteString "ElksRun" 


testCharacter = AR.armcharRes "cieran"



-- main :: IO ()
main = do 
     (g,schema,res) <- getGraph AR.characterFile AR.armFile AR.resourceFile


     --print $ TTL.formatGraphAsText $ schema
     --print $ TTL.formatGraphAsText $ res
     --print $ TTL.formatGraphAsText $ g

     -- let cl =  fromJust $ C.getAllCS g testCharacter
     -- let cmap = CM.insertListS res CM.empty $ cl
     -- let cmap = CM.insertList CM.empty $ cl
     stateVar <- getSTM res schema g 

     -- print $ C.getGameStartCharacter g $ testCharacter
     -- print $ encodePretty $ A.getIngameAdvancements g testCharacter

     -- let st = map (\ x -> show (CM.getKey x) ++ "\n" ) cl
     printTime

     print "Starting Scotty"
     S.scotty 3000 $ stateScotty stateVar

        -- HA.middleware $ basicAuth authf "armchar"
