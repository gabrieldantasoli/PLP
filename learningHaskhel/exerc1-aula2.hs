calculaAbono :: Float -> Float
calculaAbono x
    | x > 1 && x < 10     = 100
    | x >= 10 && x < 20     = 200
    | x >= 20 && x < 30     = 300
    | x >= 30 && x < 40    = 400
    | x >= 40              = 500



main :: IO()
main = do
    input <- getLine
    let abono = calculaAbono (read input)
    print abono



repeteN :: Int -> String -> IO()
repeteN 1 str = print str
repeteN n str = do
    print str 
    repeteN (n-1) str


testeRepete :: IO()
testeRepete = do
    input1 <- readLn :: IO Int
    input2 <- readLn :: IO String
    repeteN input1 input2


toBin :: Int -> String
toBin 0 = ""
toBin 1 = "1"
toBin n = toBin (n `div` 2) ++ show (n `mod` 2);