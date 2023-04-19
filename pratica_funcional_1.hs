import Text.Printf


meuimposto :: Float -> Float
meuimposto x
    | x <= 1903.98                  = 0
    | x <= 2826.65                  = 142.80 + (x - 1903.99) * 0.075
    | x <= 3751.05                  = 354.80 + (x - 2826.66) * 0.15
    
    | x <= 4664.68                  = 636.13 + (x - 3751.06) * 0.225

    | otherwise                     = 869.36 + (x - 4664.68) * 0.275


main :: IO()
main = do
    input <- readLn :: IO Float
    let imposto = meuimposto input
        liquido = input - imposto
    printf "O valor do imposto de renda retido eh: R$  %.2f" imposto
    printf "O salario liquido eh: R$ %.2f/n" liquido
