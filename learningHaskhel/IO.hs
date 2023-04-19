import Data.Char
import Control.Monad

main1 = putStrLn "Hello , World!";


main2 = do
    putStrLn "Hello , What's your name ?"
    name <- getLine
    putStrLn ("Hey , " ++ name ++ ", you rock!")


main3 = do
    foo <- putStrLn "Hello , wht's your name ?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")


main4 = do
    putStrLn "What's your first name ?"
    firstName <- getLine
    putStrLn "What's your last name ?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?";


main5 = do
    line <- getLine
    if null line then return ()
    else (do
        putStrLn $ reverseWord line
        main)
reverseWord :: String -> String;
reverseWord "" = "";
reverseWord (x:xs) = reverseWord xs ++ [x];


main6 = do 
    a <- return "aa"
    b <- return "bbbbb"
    putStrLn $ a ++ "   " ++ b


main7 = do
    print "Hahaha"
    print [1,2,3]


main8 = do  
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()


main9 = do
    c <- getChar
    when (c /= '/') $ do
        putChar c
        main


main10 = do 
    rs <- sequence [getLine, getLine, getLine]
    print rs


      
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  