module Scoring
    ( plurality
    , veto
    , borda
    , general
    ) where

import VotingSystem
import Utils(filterWinner)

import Data.List (transpose, sortOn)
import Control.Arrow((&&&))


---------------------------
-- ## Voting Systems  ## --
---------------------------

plurality :: Ord c => VotingSystem c [c]
plurality (PP c v) = general (pluralityVec $ length c) v

veto :: Ord c => VotingSystem c [c]
veto (PP c v) = general (vetoVec $ length c) v

borda :: Ord c => VotingSystem c [c]
borda (PP c v) = general (bordaVec $ length c) v


---------------------------
-- ## Scoring Vectors ## --
---------------------------

type ScoringVector = [Int]

pluralityVec :: Int -> ScoringVector
pluralityVec n = 1 : replicate (n-1) 0

vetoVec :: Int -> ScoringVector
vetoVec n = replicate (n-1) 1 ++ [0]

bordaVec :: Int -> ScoringVector
bordaVec n = [n-1,n-2..0]


----------------------------
-- ## General Protocol ## --
----------------------------

general :: Ord c => ScoringVector -> [[c]] -> [c]
general vec = filterWinner . map sumCand . transpose . map (sortOn fst . flip zip vec)
    where
        sumCand :: [(c,Int)] -> (c,Int)
        sumCand = (fst . head) &&& (sum . map snd)
