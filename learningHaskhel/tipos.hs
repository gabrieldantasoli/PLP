{-
ghci> :t 'a'  
'a' :: Char  
ghci> :t True  
True :: Bool  
ghci> :t "HELLO!"  
"HELLO!" :: [Char]  
ghci> :t (True, 'a')  
(True, 'a') :: (Bool, Char)  
ghci> :t 4 == 5  
4 == 5 :: Bool  
-}

{- DIFERENÇAS DOS TIPOS

Int = numeros inteiros , mas com limitação de tamanho . - 2.14 < Int < 2.14 (bilhões)
Integer = Int , mas sem limitação de tamanho.
Float é um número real em ponto flutuante de precisão simples.
Double é um número real em ponto flutuante com o dobro(!) de precisão.
-}

-- Int
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y +z

-- Integer
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Float
circumference :: Float -> Float
circumference r = 2 * pi * r

-- Double 
circumference' :: Double -> Double
circumference' r = 2 * pi * r 

-- Boll
andGate :: Bool -> Bool -> Bool
andGate x y = x && y