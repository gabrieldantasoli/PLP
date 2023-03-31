map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs



filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

{- Testes de filter'
 filter' (>3) [1,2,3,4,5,6,7,8,9,10]
 filter' (==3) [1,2,3,4,5,6,7,8,9,10]
 filter' (<3) [1,2,3,4,5,6,7,8,9,10]
 filter' (<=3) [1,2,3,4,5,6,7,8,9,10]
-}



quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted = quickSort(filter' (<=x) xs)
        biggerSorted  = quickSort(filter' (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

{- Testes de quickSort
    quickSort [1,2,3,4,5]
    quickSort [2,5,8,3,6,2,9,10]
-}


largestDivisible :: (Integral a) => a
largestDivisible = head (filter' p [100000,99999..])
    where p x = x `mod` 3892 == 0



getOddQuadraticSmallestThen10000 = takeWhile (<10000) (filter odd (map (^2) [1..]))