
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Spell
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Spell Records
--
--
-----------------------------------------------------------------------------
module ArM.Char.Spell ( SpellRecord(..)
                       , spellDB
                       , spellLookup
                       , SpellDB
                       ) where

import ArM.Char.Trait
import GHC.Generics
import Data.List.Split
import Text.Read
import qualified Data.Map as M

import ArM.Debug.Trace

data SpellRecord = SpellRecord
                   { spellKey :: TraitKey   -- ^ Unique identifier as used in `ArM.Char.Trait`
                   , spellRecordTeFo :: String
                   , lvl :: Maybe Int       -- ^ Spell Level.  General Level Spells have Nothing.
                   , technique :: String
                   , techniqueReq :: [String]
                   , form :: String
                   , formReq :: [String]
                   , rdt :: (String,String,String)   -- ^ Range/Duration/Target
                   , specialSpell :: [String]        -- ^ Special tags, like Ritual or Mutantum.
                   , description :: String           -- ^ Freeform description of the effect
                   , design :: String                -- ^ Level calculation
                   , comment :: String               -- ^ Freeform remarks that do not fit elsewhere
                   , cite :: String                  -- ^ Source reference
                   }
           deriving (Ord, Eq, Generic, Show)

-- | Default SpellRecord object as a starting point for step-by-step construction.
defaultSR :: SpellRecord
defaultSR = SpellRecord
                   { spellKey = OtherTraitKey "None"
                   , spellRecordTeFo = ""
                   , lvl = Nothing
                   , technique = ""
                   , techniqueReq = []
                   , form = ""
                   , formReq = []
                   , rdt = ("Per","Mom","Ind")
                   , specialSpell = []
                   , description = ""
                   , design = ""
                   , comment = ""
                   , cite = ""
                   }

type SpellDB = M.Map TraitKey SpellRecord

-- | Create a `Data.Map.Map` of SpellRecord objects.  
-- The input is the output from `Data.CSV.csvFile`
spellDB :: [[String]] -> SpellDB
spellDB = M.fromList . map ( \ x -> (spellKey x,x) ) . map fromCSVline

-- | Parse the cells of one line from the CSV file into a SpellRecord object.
fromCSVline :: [String] -> SpellRecord
fromCSVline (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:_) =
      defaultSR { spellKey = SpellKey x1 
                , spellRecordTeFo = x2
                , lvl = readMaybe x7
                , technique = x3
                , techniqueReq = filter (/="") $ splitOn ";" x4
                , form = x5
                , formReq = filter (/="") $ splitOn ";" x6
                , rdt = (x8, x9, x10)
                , specialSpell =  filter (/="") $ splitOn ";" x11
                , description = x12
                , design = x13
                , comment = x14
                , cite = x15
                }
fromCSVline _ = defaultSR

spellLookup :: TraitKey -> SpellDB -> Maybe SpellRecord
spellLookup = M.lookup 



