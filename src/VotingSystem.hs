module VotingSystem
    ( VotingSystem
    , PrefProfile(PP)
    , canditates
    , votes
    ) where

data PrefProfile c t = PP {
        canditates :: [c],
        votes :: [t]
    }

type VotingSystem c t = PrefProfile c t -> [c]