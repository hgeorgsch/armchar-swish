{-# LANGUAGE OverloadedStrings #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.JSON
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Functions to produce 
--
-----------------------------------------------------------------------------
module ArM.JSON where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Key
import Swish.RDF.Graph
import Swish.Namespace
-- import ArM.Query
import ArM.Character
import ArM.KeyPair
import ArM.Resources
import Data.Maybe
import Network.URI (URI)
import qualified Data.Text as T
import Swish.RDF.Vocabulary.XSD
import Data.List (intercalate)
import Data.List.Split (splitOn)


import qualified Data.Aeson.KeyMap as KM


-- We need to rethink the use of triples here, as it is not
-- one-to-one and contains redundant data.
-- Do we need the label from the (property,label,value) triple?

-- We may want to make an algebraic datatype to replace Triple
-- See here for decoding tips
-- https://stackoverflow.com/questions/53478455/aeson-parse-json-object-to-list
--
-- Another comprehensive tutorial:
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

tripleToJSON (KeyValuePair a b) = 
    ((getKey $ fromJust $ fromRDFLabel a), (toJSON b))

getKey :: RDFLabel -> Key
getKey = fromString  . show

labelToData :: RDFLabel -> Either KVP (Either Int String)
labelToData l | i /= Nothing = Right $ Left (fromJust i)
              | s /= Nothing = Right $ Right (fromJust s)
              | uri /= Nothing = Left $ KVP (show $ fromJust uri)
              | otherwise    = Right (Right (show l))
    where  s = fromRDFLabel l :: Maybe String
           i = fromRDFLabel l :: Maybe Int
           uri = fromRDFLabel l :: Maybe ScopedName


data KeyPairList  = KeyPairList [KeyValuePair]

data KVP = KVP { prefixedid :: String }
   deriving (Show,Eq)
instance ToJSON KVP where 
    toJSON k = object [ fromString "prefixedid" .= toJSON ( prefixedid k ) ]
instance FromJSON KVP where 
    parseJSON (Object x) = KVP <$> x .: "prefixedid"

instance ToJSON RDFLabel where
    toJSON  = f . labelToData
        where f (Left x) = toJSON x
              f (Right (Left x)) = toJSON x
              f (Right (Right x)) = toJSON x

instance FromJSON RDFLabel where
   parseJSON (Number x) = return $ TypedLit (T.pack $ show  x) xsdInteger
   parseJSON (String x) = return $ Lit x
   parseJSON x = fmap kvpToRDFLabel $ parseJSON x
      where kvpToRDFLabel k = f x $ intercalate "" xs
                where (x:xs) = splitOn ":" $ prefixedid k
            f "arm" = Res . makeSN 
            f "armchar" = armcharRes
            f "armr" = armrRes

-- instance FromJSON KeyPairList  where
  -- parseJSON = withObject "KeyPairList" $ \obj ->
    -- let kvs = KM.toList obj
        -- parsed = mapM pairToKeyValue kvs
    -- in fmap KeyPairList parsed

-- pairToKeyValue (x,y) = do
    -- v <- parseJSON y
    -- return $ KeyValuePair k v
    -- where k = Blank $ toString x

instance ToJSON Trait where 
    toJSON t = object $ map tripleToJSON (traitContents t) 

-- instance FromJSON Trait where 
    -- parseJSON cs = object (c:x:xs)
    -- parseJSON val = withObject "Test"
                               -- (\o -> Test <$> mapM parseJSON (elems o))
                               -- val


instance ToJSON CharacterSheet where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (csTraits cs))
             xs = map tripleToJSON (csMetadata cs)
             c = (fromString "arm:isCharacter") .= (show $ csID cs)
    
instance ToJSON Advancement where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (traits cs))
             xs = map tripleToJSON (contents cs)
             c = (fromString "id") .= (advancementIDstring cs)

