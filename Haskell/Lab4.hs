powS:: Integer->Integer->Integer
powS x 0 = 1
powS x y = x*powS x (y-1)

powT:: Integer->Integer->Integer->Integer
powT x 0 acc = acc
powT x y acc = powT x (y-1) (acc*x)

fiboS:: Integer->Integer
fiboS n = if n<=2 then 1
          else fiboS (n-1) + fiboS (n-2)
          
fiboT:: Integer->Integer->Integer->Integer
fiboT n f1 f2 = if n<=2 then f2
                else fiboT (n-1) f2 (f1+f2)
                
prodS:: [Integer]->Integer
prodS []=1
prodS (x:xs) = x*prodS xs

prodT:: [Integer]->Integer->Integer
prodT [] acc = acc
prodT (x:xs) acc = prodT xs (acc*x)

search:: [Integer]->Integer->Bool
search [] n = False
search (x:xs) n = if x/=n then search xs n
                  else True 
