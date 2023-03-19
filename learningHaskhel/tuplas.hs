-- Tuplas não precisam ser homogêneas. Ao contrário de uma lista, uma tupla pode conter uma combinação de vários tipos. 

tupla1 = (1,'2',3,"gabriel")
matriztupla1 = [(1,2),(3,4),(5,6)]

getFirstDoubleTuple = do
    print(fst (matriztupla1 !! 0))

getSecondDoubleTuple = do
    print(snd (matriztupla1 !! 0))

-- Forma tuplas pares mesclanco os elementos da primeira lista com os da segunda (zip)
zipMe x y = do
    print(zip x y)

triangules = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]  
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  