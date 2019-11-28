module VotingSystem
    ( VotingSystem
    ) where

data PrefProfile c t = PP {
        canditates :: [c],
        votes :: [t]
    }

type VotingSystem c t = PrefProfile c t -> [c]