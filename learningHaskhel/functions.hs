
-- funções criadas
doubleMe x = x * 2
tripleMe y = y * 3
myExp x y = x ** y
doubleUs x y = x*2 + y*2
doubleUs2 x y = doubleMe x + doubleMe y

-- funções usando outras funções
main = do
  print (doubleMe 2)
  print (tripleMe 2)
  print (myExp 2 3)

ept = do
    print(10 `myExp` 3)

doubleUsSum = do
    print(doubleUs 10 5)

doubleUsFunc = do
    print(doubleUs2 10 5)