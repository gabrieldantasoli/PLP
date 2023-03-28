
maximun' :: (Ord a) => [a] -> a
maximun' [] = error "maximun of empty list"
maximun' [x] = x
maximun' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximun' xs

{-
    maximum' :: (Ord a) => [a] -> a  
    maximum' [] = error "maximum of empty list"  
    maximum' [x] = x  
    maximum' (x:xs) = max x (maximum' xs)  
-}



replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x



take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0  = []
take' _ []    = []
take' n (x:xs) = x : take' (n-1) xs



get' :: (Num i , Ord i) => i -> [a] -> a
get' n x
    | n <= 0    = error "error"
get' n (x:xs)
    | n <= 1    = x
    | otherwise = get' (n - 1) xs 


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


bubblesort :: (Ord a) => [a] -> [a]  
bubblesort [] = []  
bubblesort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  