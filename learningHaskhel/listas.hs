-- Em Haskell, listas são estruturas de dados homogêneas. Ela armazena vários elementos do mesmo tipo.

oddNumbers = [1,3,5,7,9];
evenNumbers = [2,4,6,8,10];

-----------------------------------------------------------
--Quando juntamos duas listas (mesmo se você adicionar uma lista de um elemento só em outra lista, como por exemplo:[1,2,3] ++ [4]), internamente o Haskell irá percorrer toda a lista que esta do lado esquerdo do ++. Isto não é um problema quando não estamos lidando com listas muito grandes.

naturals = oddNumbers ++ evenNumbers;

-- No entanto, colocar alguma coisa no início de uma lista utilizando o operador : (também chamado de contra operador) será instantâneo. 

addStart = 1 : [2,3,4];
addingNumber =  1:2:3:4:5: [6];

-----------------------------------------------------------
-- Acessar um elemento de uma lista pelo seu index
name = "Gabriel Dantas"
firstNamesLetter = name !! 0

-----------------------------------------------------------
-- Matrizes : listas contendo listas

-- Para Acessar elementos de um matriz , usaremos !!. Exemplo: matriz1 !! 0 = [1,2,3]. Já matriz1 !! 0 !! 0 = 1
matriz1 = [[1,2,3],[4,5,6],[7,8,9]]

showMatriz = do
    print(matriz1 !! 0)
    print(matriz1 !! 1)
    print(matriz1 !! 2)

-- matrizes podem receber listas 
-- adicionando uma lista no inicio da matriz
matrizLista = [-2,-1,0] : matriz1

-- adicionando uma lista no fim da matriz
novaLinha = [10,11,12]
matriz2 = matriz1 ++ [[10,11,12]]

-- tamanho de uma listas : length lista
-----------------------------------------------------------

--operações em listas : head , tail , last , init. 
-- Outras operações : length , null , reverse , take , drop , maximum , minimum , sum , product , elem.

lista = [0,1,2,3,4,5,6,7,8,9];

showOperations = do
    print(head lista)
    print(tail lista)
    print(last lista)
    print(init lista)

isEmpty x = length x == 0










{- (comentário de multiplas linhas)
ghci> "hello" ++ " " ++ "world"  
    "hello world"  
ghci> ['w','o'] ++ ['o','t']  
    "woot"  
-}
