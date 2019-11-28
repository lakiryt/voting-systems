module VotingSystem
    ( VotingSystem(vote)
    ) where

type VotingSystem c t = PrefProfile c t -> [c]