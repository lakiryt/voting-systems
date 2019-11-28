module VotingSystem
    ( VotingSystem(vote)
    ) where

class VotingSystem t where
    vote :: Enum c => c -> t -> [c]