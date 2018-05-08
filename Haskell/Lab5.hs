duplT:: [Integer]->[Integer]->[Integer]
duplT [] acc = acc
duplT (x:xs) acc = if elem x xs then duplT xs acc else duplT xs (x:acc)

              
duplS:: [Integer]->[Integer]
duplS [] = []
duplS (x:xs) = if (elem x xs) then duplS xs
               else x:duplS xs
               
prime:: Integer->Integer->Bool
prime x acc = if acc > (div x 2) then True
              else if mod x acc==0 then False
              else prime x (acc+1)

f3r:: Integer->[Integer]->[Integer]
f3r n acc = if n==0 then acc
            else if prime n 2 then f3r (n-1) (n:acc)
            else f3r (n-1) acc
            
f3lc:: Integer->[Integer]
f3lc n = [x | x <- [1..n], prime x 2==True]

sum_of_digits ::Integer->Integer
sum_of_digits x=if x>0 then mod x 10 + sum_of_digits (div x 10)
                else 0
                
f4:: Integer->Integer->Bool
f4 x k = if sum_of_digits x==k then True
         else False
         
f5r:: Integer->Integer->Integer->[Integer]->[Integer]
f5r x1 x2 k acc= if x1>x2 then acc
                else if f4 x1 k then f5r (x1+1) x2 k (x1:acc)
                else f5r (x1+1) x2 k acc
                
f5lc::Integer->Integer->Integer->[Integer]
f5lc x1 x2 k=[x | x<-[x1..x2], f4 x k ==True]
