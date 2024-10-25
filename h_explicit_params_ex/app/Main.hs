{-# LANGUAGE ImplicitParams #-}

import Data.Function (on)

-- Define a sort function that uses an implicit comparator
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (p:xs) = sortBy cmp (filter ((== GT) . cmp p) xs) ++ [p] ++ sortBy cmp (filter ((/= GT) . cmp p) xs)

-- Define a sort function using an implicit parameter for the comparator
sort :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort = sortBy ?cmp

main :: IO ()
main = do
    let ?cmp = compare  -- Define the implicit comparator
    let xs = [5, 3, 8, 1, 2]
    
    print $ sort xs  -- Outputs: [1,2,3,5,8]
    print $ sort [3,2,1]  
