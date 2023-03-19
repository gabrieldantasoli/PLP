module MyFunctions where

doubleMe x = x * 2
tripleMe y = y * 3
myExp x y = x ** y

main = do
  print (doubleMe 2)
  print (tripleMe 2)
  print (myExp 2 3)

ept = do
    print(myExp 10 3)