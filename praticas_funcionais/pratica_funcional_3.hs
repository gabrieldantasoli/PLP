import Text.Printf

removeRepetidos :: [Int] -> [Int] -> [Int]
removeRepetidos [] lista = []
removeRepetidos (x:xs) lista
    | x `elem` lista = removeRepetidos xs lista
    | otherwise = x : removeRepetidos xs (x:lista)

countrepetidos :: [Int] -> [Int] -> IO()
countrepetidos [] _ = return()
countrepetidos (x:xs) lista = do
    let qtdrepeticoes = length (filter (== x) lista)
    printf "%d foi sorteado %d vez(es)\n" x qtdrepeticoes
    countrepetidos xs lista

main :: IO ()
main = do
    lista <- readLn :: IO [Int]
    if lista == [] then printf "neh" else countrepetidos (removeRepetidos lista []) lista
