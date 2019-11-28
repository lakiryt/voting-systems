module Utils
    ( filterWinner
    ) where


filterWinner :: [(c, Int)] -> [c]
filterWinner = map fst . maxOn snd
        
maxOn :: (b -> Int) -> [b] -> [b]
maxOn = maxOnAux 0 []
    where
        maxOnAux :: Int -> [b] -> (b -> Int) -> [b] -> [b]
        maxOnAux curMax ac f []     = ac
        maxOnAux curMax ac f (x:xs)
            | f x > curMax  = maxOnAux (f x) [x]     f xs
            | f x == curMax = maxOnAux curMax (x:ac) f xs
            | otherwise     = maxOnAux curMax ac     f xs
