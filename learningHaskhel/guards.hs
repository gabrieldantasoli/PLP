bmiTell :: (RealFloat a) => a -> String
-- cometario: bmi = a 
bmiTell bmi
    | bmi <= 18.5 = "Voce esta abaixo do peso!"
    | bmi <= 25.0 = "Supostamente , voce esta normal."
    | bmi <= 30.0 = "Voce esta acima do peso!"
    | otherwise = "Baleia a vista!"

bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
    | bmi <= skinny = "Você esta abaixo do peso!"  
    | bmi <= normal = "Supostamente você esta normal. Pfff, aposto que você é feio!"  
    | bmi <= fat = "Você esta gordo! Faça uma dieta, gorducho!"  
    | otherwise   = "Você é uma baleia, meus parabéns!"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)


max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b


myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT


cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

threePower :: (RealFloat a) => a -> a -> a -> (a,a,a)
threePower a b c = let power x = x ^ 2 in (power a, power b, power c)