multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9

multTwoWithEighteen = multTwoWithNine 2

compareWithHundread :: (Num a, Ord a) => a -> Ordering
compareWithHundread x = compare x 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


isAlphanum :: Char -> Bool
isAlphanum = (`elem` ['A'..'Z'])


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y