lucky :: (Integral a) => a -> String
lucky 7 = "SETE! BINGO!"
lucky 13 = "13. FAZUELI"
lucky x = "Azarado para porra!"


sayMe :: (Integral a) => a -> String
sayMe 1 = "Um!"
sayMe 2 = "Dois!"
sayMe 3 = "Tres!"
sayMe 4 = "Quatro!"
sayMe 5 = "Cinco!"
sayMe x = "Nao esta entre 1 e 5."


fatorial :: (Integral a) => a -> a
fatorial 0 = 1;
fatorial n = n * fatorial (n - 1)


charName :: Char -> String
charName 'g' = "Gabriel"
charName 's' = "samuel"
charName 'a' = "alda"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

head' :: [a] -> a
head' [] = error "error"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Lista Vazia!"
tell (x:[]) = "A lista tem um elemento : " ++ show x
tell (x:y:[]) = "A lista tem dois elementos : " ++ show x ++ " e " ++ show y
tell (x:y:_) = "Esta lista tem mais de dois elementos"