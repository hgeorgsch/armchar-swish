{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Markdown
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Classes and instances to make MarkDown output.
--
-- The core of this module is the `Markdown` class and its `printMD`
-- function which renders an object in Markdown.
--
-----------------------------------------------------------------------------
module ArM.Char.Markdown ( Markdown(..)
                         , artMD
                         , gameStartSheet
                         , currentSheet
                         , pListMD
                         , sortArts
                         , printCastingTotals
                         , briefTraits
                         ) where

import Data.Maybe 
import Data.List 
import qualified Data.Map as M

import ArM.Char.Internal.Character 
import ArM.Char.CharacterSheet
import ArM.Char.Trait
import ArM.Char.Advancement
import ArM.Char.Spell
import ArM.GameRules
import ArM.BasicIO

-- import ArM.Debug.Trace

-- | Class defining `printMD` to render in Markdown.
class Markdown a where

     -- | This is the basic function to render in Markdown
     printMD' :: a           -- ^ object to render
              -> [ String ]  -- ^ list of lines for output
     printMD :: a           -- ^ object to render
             -> OList       -- ^ list of lines for output
     printMD = toOList . printMD'

     -- | This is a hack to augment characters using extra resources
     -- By default, it is identical to `printMD`.
     printMDaug' :: SpellDB    -- ^ Database of Spell information
                -> a          -- ^ object to render
                -> [ String ] -- ^ list of lines for output
     printMDaug' _ = printMD'
     printMDaug :: SpellDB    -- ^ Database of Spell information
                -> a          -- ^ object to render
                -> OList      -- ^ list of lines for output
     printMDaug _ = printMD

instance Markdown FieldValue where
   printMD' =  (:[]) . show
   printMD  = OString . show

instance Markdown KeyPair where
   printMD' (KeyPair x  y) = [ x, ':':' ':show y, "" ]
   printMD (KeyPair x  y) = OList
         [ OString x
         , OString $ ':':' ':show y
         , OString "" ]
instance Markdown KeyPairList where
   printMD' (KeyPairList xs) = ( foldl (++) [] $ map printMD' xs )
   printMD (KeyPairList xs) = OList $ map printMD xs
instance Markdown CharacterConcept where
   printMD c = OList
               [ OString ("# " ++ fullConceptName c )
               , OString ""
               , OString $ show (charType c)
               , OString $ ": " ++ ( fromMaybe "-" $ briefConcept c )
               , OString ""
               , OString "Quirk"
               , OString $ ": " ++ ( fromMaybe "-" $ quirk c )
               , OString ""
               , OString "Appearance" 
               , OString $ ": " ++ ( fromMaybe "-" $ appearance c )
               , OString ""
               , OString "Born" 
               , OString brn
               , OString ""
               , OString "Player" 
               , OString $ ": " ++ ( fromMaybe "-" $ player c )
               , OString ""
               , ( printMD $ charGlance c ) 
               , ( printMD $ charData c )
               ]
          where brn | born c == Nothing = ": ??" 
                    | otherwise = ": " ++ (show $ fromJust $ born c)
   printMD' c = ("# " ++ fullConceptName c ):""
      : typ : con : ""
      : "Quirk" : qrk : ""
      : "Appearance" : app : ""
      : "Born" : brn : ""
      : "Player" : ply : ""
      : ( printMD' $ charGlance c ) ++ ( printMD' $ charData c )
          where typ = show (charType c)
                con = ": " ++ ( fromMaybe "-" $ briefConcept c )
                qrk = ": " ++ ( fromMaybe "-" $ quirk c )
                app = ": " ++ ( fromMaybe "-" $ appearance c )
                ply = ": " ++ ( fromMaybe "-" $ player c )
                brn | born c == Nothing = ": ??" 
                    | otherwise = ": " ++ (show $ fromJust $ born c)

-- | Apply `printMD` to every object in the list and merge the results.
listMD :: Markdown a => [a]    -- ^ objects to render
                     -> [String] -- ^ list of lines of Markdown output
listMD = foldl (++) [] . map printMD'

-- | Render every object in the list and apply a header at the start.
pListMD :: Markdown a => String  -- ^ Header string
                      -> [a]     -- ^ objects to render
                      -> [String] -- ^ list of lines of Markdown output
pListMD _ [] = []
pListMD s x = ("":s:"":listMD x)

-- | Render art scores as a table
artMD :: CharacterSheet
      -> [ String ]
artMD = ("":) . (h1:) . (h2:) . map artLine . sortArts . artList 
   where h1 = "| Art  | Score | XP |" 
         h2 = "| -: | -: | -: |"

-- | List of arts defined in *Ars Magica*
arts :: [ String ]
arts = [ "Creo", "Intellego", "Muto", "Perdo", "Rego",
         "Animal", "Aquam", "Auram", "Corpus", "Herbam", 
         "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

-- | Map assigning sort index to each Art
sMap :: M.Map String Int 
sMap = M.fromList $ zip arts [1..15]

-- | Sort a list of arts in canonical order
sortArts :: [Art] -> [Art]
sortArts = sortOn ( fromMaybe 0 . ( \ x -> M.lookup x sMap ) . artName)

-- | Auxiliary for `artMD`, rendering a single line in the table
artLine :: Art -> String
artLine ar = "| " ++ artName ar  ++ " | " ++ show (artScore ar) ++ " | " ++ show (artExcessXP ar) ++ " |"

showlistMD :: Show a => String -> [a] -> OList
showlistMD _ [] = OList []
showlistMD s xs = OList [ OString s
                        , toOList $ (map (++", ") $ map show xs)
                        ]

instance Markdown CharacterSheet where
   printMD c = OList [ cl, ml, lt ]
    where ml = OList
               [ showlistMD "+ **Characteristics:** "  $ charList c
               , showlistMD "+ **Personality Traits:** "  $ ptList c
               , showlistMD "+ **Reputations:** "  $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ vfList c
               , showlistMD "+ **Abilities:** "  $ abilityList c
               , showlistMD "+ **Arts:** "  $ sortArts $ artList c
               , showlistMD "+ **Spells:** "  $ spellList c
               ]
          -- artl = artMD c
          lt = toOList $ printCastingTotals c
          cl = toOList $ briefTraits c
   printMD' c = (cl ++ ml ++ lt ) -- foldl (:) ml cl
    where f _ [] = ""
          f s xs = foldl (++) s (map (++", ") $ map show xs)
          ml = [ f "+ **Characteristics:** "  $ charList c
               , f "+ **Personality Traits:** "  $ ptList c
               , f "+ **Reputations:** "  $ reputationList c
               , f "+ **Virtues and Flaws:** "  $ vfList c
               , f "+ **Abilities:** "  $ abilityList c
               , f "+ **Arts:** "  $ sortArts $ artList c
               , f "+ **Spells:** "  $ spellList c
               ]
          -- artl = artMD c
          lt = printCastingTotals c
          cl = briefTraits c
   printMDaug' db = printMD' . addCastingScores db

briefTraits :: CharacterSheet -> [String]
briefTraits c = ag:(ol++cl)
     where
          cl = foldl (++) [] $ map printMD' $ confList c
          ol = foldl (++) [] $ map printMD' $ csTraits c
          ag = "+ **Age:** " ++ show (csAge c)
printCastingTotals :: CharacterSheet -> [String]
printCastingTotals c 
             | Magus /= csType c = []
             | otherwise = "":"| Casting Total | Creo | Intellego | Muto | Perdo | Rego |":
                              "|         :-    |  -:  |  -:       |  -:  |  -:   |  -:  |":
                              lts
      where
          lts = [ "| " ++ fo ++ foldl (++) "" (map ( (" | "++) . show ) ts ) ++ " |" 
                | (fo,ts) <- zip lforms (labTotals c) ]
          lforms = [ "Animal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

instance Markdown Confidence where
   printMD' c = [ "+ **" ++ cname c ++ "**: " ++ show (cscore c) ++ " ("
             ++ show (cpoints c) ++ ")" ]
   printMD c = OString $
             "+ **" ++ cname c ++ "**: " ++ show (cscore c) ++ " ("
             ++ show (cpoints c) ++ ")" 
instance Markdown OtherTrait where
   printMD' c = [ "+ **" ++ trait c ++ "**: " ++ show (otherScore c) ++ " ("
             ++ show (otherExcess c) ++ ")" ]
   printMD c = OString $
             "+ **" ++ trait c ++ "**: " ++ show (otherScore c) ++ " ("
             ++ show (otherExcess c) ++ ")" 

-- | Render a character sheet at game start.
-- Unlike the regular `printMD`, this includes only the character design
-- and not ingame advancements.
gameStartSheet :: SpellDB -> Character -> [String]
gameStartSheet db c = ( printMD' . concept ) c 
            ++ maybeP (state c)
            ++ (pListMD "## Game start design" as')
       where 
             as' = pregameDesign c
             maybeP Nothing = []
             maybeP (Just xs) = printMDaug' db xs

-- | Render the current character sheet without pregame design details.
currentSheet :: SpellDB -> Character -> [String]
currentSheet db c = ( printMD' . concept ) c 
            ++ maybeP (state c)
            ++ (pListMD "## Past Advancement" bs)
            ++ (pListMD "## Future Advancement" cs)
       where 
             bs = pastAdvancement c
             cs = futureAdvancement c
             maybeP  Nothing = []
             maybeP (Just xs) = printMDaug' db xs
 
instance Markdown Character where
   printMD' c = ( printMD' . concept ) c 
            ++ maybeP (state c)
            ++ (pListMD "## Game start design" as')
            ++ (pListMD "## Pregame Development" as)
            ++ (pListMD "## Past Advancement" bs)
            ++ (pListMD "## Future Advancement" cs)
       where 
             as' = pregameDesign c
             as = pregameAdvancement c
             bs = pastAdvancement c
             cs = futureAdvancement c
             maybeP  Nothing = []
             maybeP (Just xs) = printMD' xs
   printMDaug' db c = ( printMD' . concept ) c 
            ++ maybeP (state c)
            ++ (pListMD "## Game start design" as')
            ++ (pListMD "## Pregame Development" as)
            ++ (pListMD "## Past Advancement" bs)
            ++ (pListMD "## Future Advancement" cs)
       where 
             as' = pregameDesign c
             as = pregameAdvancement c
             bs = pastAdvancement c
             cs = futureAdvancement c
             maybeP  Nothing = []
             maybeP (Just xs) = printMDaug' db xs

instance Markdown CharacterState where
   printMD c = OList
       [ OString $ "## " ++ (show $ charTime c )
       , OString ""
       , printMD $ filterCS c
       ]
   printMDaug db c = OList
       [ OString $ "## " ++ (show $ charTime c) 
       , OString ""
       , printMDaug db $ filterCS c
       ]
   printMD' c = ( "## " ++ (show $ charTime c) ):"":sh
       where sh = printMD' $ filterCS c
   printMDaug' db c = ( "## " ++ (show $ charTime c) ):"":sh
       where sh = printMDaug' db $ filterCS c

-- | Render the source quality of an advancement
showSQ :: Maybe XPType -> Maybe XPType -> String
showSQ Nothing Nothing = ""
showSQ (Just x) Nothing = " (" ++ show x ++ "xp)"
showSQ Nothing (Just x) = " (" ++ show x ++ "xp)"
showSQ (Just x) (Just y) = " (" ++ show x ++ f (y-x) ++ "xp)"
    where f 0 = ""
          f z = "+" ++ show z

instance Markdown AugmentedAdvancement where
   printMD' a = showTime xps (season a) (mode a) y : (fn (narrative a) $ pt a)
      where xps = showSQ (sourceQuality a) (effectiveSQ a)
            cs = map ("    + "++) . foldl (++) [] . map printMD' . changes 
            vs = map ("    + "++) . map show . validation
            pt x = (cs x) ++ (vs x)
            fn Nothing xs = xs
            fn (Just x) xs = ( "    + " ++ show ( x ) ) :xs
            y = augYears a

-- | Render the season and mode of an advancement
showTime :: String -> SeasonTime -> Maybe String -> Maybe Int -> String
showTime xps NoTime Nothing y = ("+ ?? " ++ xps ++ showYears y)
showTime xps  x Nothing y = ("+ " ++ show x ++ xps ++ showYears y)
showTime xps NoTime (Just x) y = ("+ " ++ x ++ xps ++ showYears y)
showTime xps x (Just z) y = ("+ " ++ show x ++ xps ++ showYears y ++ " " ++ z)

-- | Render the duration of an advancement
showYears :: Maybe Int -> String
showYears Nothing = ""
showYears (Just x) = " (" ++ show x ++ " years)"

instance Markdown Advancement where
   printMD' a = showTime xps (season a) (mode a) y : (fn (narrative a) $ pt a)
      where xps | sx == Nothing = ""
                | otherwise = " (" ++ ishow  sx ++ "xp)" 
            sx = sourceQuality a
            ishow = show . fromJust
            pt = map ("    + "++) . foldl (++) [] . map printMD' . changes 

            fn Nothing xs = xs
            fn (Just x) xs = ( "    + " ++ show ( x ) ) :xs
            y = advYears a
instance Markdown Trait where
   printMD' c = [ show c ]
   printMD = OString . show 
instance Markdown ProtoTrait where
   printMD' c = [ show c ]
   printMD = OString . show 
