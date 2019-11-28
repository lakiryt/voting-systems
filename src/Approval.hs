module Approval
    ( approval
    ) where

import VotingSystem
import Utils(filterWinner)

import Data.List (transpose)

approval :: VotingSystem c [Int]
approval (PP c v) = filterWinner $ zip c $ map sum $ transpose v