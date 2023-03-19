doubleMe x = x * 2

doubleSmallestThen100 x = if x < 100 then doubleMe x else x;

didIPassed x = if x >= 7 then "YES" else if x < 4 then "NOT" else "FINAL"

main = do 
    print(doubleSmallestThen100 80)

didI = do
    print(didIPassed 3.9)
    print(didIPassed 4)
    print(didIPassed 6.9) 
    print(didIPassed 7)
    print(didIPassed 10)