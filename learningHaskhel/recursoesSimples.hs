length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

showLista :: (Show a) => [a] -> String
showLista [] = ""
showLista (a:sx) = show a ++ " " ++ showLista sx

sumLista :: (Num a) => [a] -> a
sumLista [] = 0
sumLista (x:xs) = x + sumLista xs

capital :: String -> String
capital "" = "String vazia. Opps!"
capital all@(x:xs) = "A primeira letra de " ++ all ++ " = " ++ [x] ++ " e o reto = " ++ xs