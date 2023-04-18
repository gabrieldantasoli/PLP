par:: Int -> Bool
par x = x `mod` 2 == 0;

soma :: Int -> Int -> Int
soma x y = x + y;

incrementa :: Int -> Int
incrementa x = x + 1;

decrementa :: Int -> Int
decrementa x = x - 1;

dobro :: Float -> Float
dobro x = x * 2;

quad :: Float -> Float
quad x = dobro (dobro x)

abs' :: Float -> Float
abs' x = if x >= 0 then x else x;

menor :: Int -> Int -> Int
menor x y = if x < y then x else y;


classes :: Int -> String
classes x 
    | x < 0          = "Negativo"
    | x < 100        = "Bronze"
    | x < 1000       = "Prata"
    | x < 10000      = "Ouro"
    | otherwise      = "Platina"


conceito :: Float -> Char
conceito x 
    | x < 4     = 'E'
    | x < 6     = 'D'
    | x < 7.5   = 'C'
    | x < 9     = 'B'
    | otherwise = 'A'


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2);


imc :: Float -> Float -> String
imc p a
    | imc' <= 18.5    = "Abaixo do peso!"
    | imc' <= 25      = "Regular!"
    | otherwise       = "Acima do peso!"
    where imc' = p / (a**2)


findString :: String -> Char -> String 
findString [] _ = ""
findString (x:xs) c
    | x == c     = [x]
    | otherwise  = findString xs c