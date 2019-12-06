module Pairs where

import VotingSystem

import qualified Data.Map.Strict as M
import Control.Monad(zipWithM,join)
import Data.List(transpose,sort)
import Data.Maybe(maybeToList)




---------------------------
-- ## Präferenzliste ## --
---------------------------

type PreferenceOver c = M.Map c Int

listToPreference :: Ord c => [c] -> PreferenceOver c
listToPreference = M.fromList . flip zip [1..]

-- Unsafe lookup
find :: Ord k => k -> M.Map k a -> a
find = M.findWithDefault undefined


--------------------------
--   ##    Vote    ##   --
--------------------------
paare dom = [(a,b) | a<-dom, b<-dom, a<b]

-- vote :: Ord c => Int -> [[c]] -> Maybe c
-- vote :: Int -> [[Essen]] -> Maybe Essen
vote :: Ord a => VotingSystem a [a]
vote (PP c v) =
        maybeToList $
        join $
        -- Finde Gewinner
        fmap (countN $ length c - 1) $
        -- Bestimme Gewinner der Spalte:
        zipWithM count2 (paare c) $
        transpose $
        map (
            -- Erstelle eine Zeile der Tabelle
            (\pref -> map (vergleich pref) (paare c)) .
            listToPreference
        ) v

-- Vergleicht a und b bezüglich der Präferenz mp.
-- c ist der Typ der Kandidaten.
-- (Erstellen einer "Zelle" der Tabelle) 
vergleich :: Ord c => PreferenceOver c -> (c, c) -> c
vergleich mp (a,b) = if find a mp < find b mp then a else b

-- Erwarte Liste aus a und b.
-- Zähle was (echt) öfter in xs vorkommt.
count2 :: Eq c => (c,c) -> [c] -> Maybe c
count2 (a,b) xs =
    let (half,r) = length xs `divMod` 2
    in  case length (filter (a ==) xs) `compare` half of
            GT -> Just a
            LT -> Just b
            _  -> if r == 0 then Nothing else Just b

-- Ermittelt Element aus xs, das als erstes öfter als n-mal vorkommt.
countN :: Ord c => Int -> [c] -> Maybe c
countN n xs =
    let h:sorted = sort xs
    in  countNAux 1 h sorted
    where
        countNAux nac xac xs
            | nac >= n   = Just xac
            | null xs   = Nothing
            | head xs /= xac    = countNAux    1    (head xs) (tail xs)
            | otherwise         = countNAux (nac+1) (head xs) (tail xs)
    