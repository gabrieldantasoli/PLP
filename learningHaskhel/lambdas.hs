{-
 map (+3) [1,6,3,2] e map (\x -> x + 3) [1,6,3,2] são equivalentes desde que ambos (+3) e (\x -> x + 3) sejam funções que peguem um número e adicione 3 nele

 map (\(a,b) -> a + b) [(1,2),(2,3),(3,4)]
-}


addThree1 :: (Num a) => a -> a -> a -> a
addThree1 a b c = a + b + c

addThree2 :: (Num a) => a -> a -> a -> a
addThree2 = \a -> \b -> \c -> a + b + c