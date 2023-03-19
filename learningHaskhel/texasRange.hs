-- Da seguinte forma , podemos declarar listas rapidamente

-- Texas Range 
oneToTwenty = [1..20]
twentyToOne = [20,19..1]
multiplesofTwo = [0,2..100]
multiplesofThree = [0,3..100]

-- take , cycle , repeat , replicate  
multiplesofThirteen = take 24 [13,26..]
infiniteLOL = take 100 (cycle "LOL")
infinite123 = take 24 (cycle [1,2,3])
tenOfFives = take 10 (repeat 5)
fiveTens = replicate 5 10

-- Alphabet Range
lowerAlphabet = ['a'..'z']
upperAlphabet = ['A'..'Z']

-- Compreensão de lista
multOfTwo = [x*2 | x <- [1..10]]
twentyTo1 = [x | x <- [20,19..1]]

-- filtragem 
multOfTwoGreatestThen10 = [x*2 | x <- [1..10], x*2 > 10]
numberMod7Equals3 = [x | x <- [0..100], x `mod` 7 == 3]
boomBang xs = [if odd x then "BOMM!" else "BANG!" | x <- xs]

not131517 xs = [x | x <- xs, x /= 13, x /= 15, x /= 19]
greatestThen50 xs xy = [x*y | x <- xs, y <-xy, x*y > 50]

substantivos = ["gerente","programador","cliente"]  
adjetivos = ["malemolente","chato","fofoqueiro"]  

concatenaSubsAdj xs xy = [x ++ " " ++ y | x <- xs, y<- xy]

myLength xs = sum [1 | _ <- xs]