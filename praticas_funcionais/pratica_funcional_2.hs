import Control.Monad (replicateM)

create :: Int -> IO [String]
create n = replicateM n getLine

valorConta :: [String] -> Int
valorConta itens = sum (map valoritem itens)

valoritem :: String -> Int
valoritem x
    | x == "cafe"             = 4
    | x == "pao"              = 2
    | x == "suco"             = 5
    | x == "pao de queijo"    = 5
    | x == "sanduiche"        = 3
    | otherwise               = 0
    
main :: IO()
main = do
    qtdItens <- readLn :: IO Int
    itens <- create qtdItens
    print (valorConta itens)