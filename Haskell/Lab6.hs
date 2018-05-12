lengthT:: [Int]->Int->Int
lengthT [] acc = acc
lengthT (x:xs) acc = lengthT xs (acc+1)

lengthW:: [Int]->Int
lengthW (x:xs) = length (x:xs) acc where
    acc=0
    length [] acc = acc
    length (x:xs) acc = length xs (acc+1)
  
search:: [Int]->Int->Bool
search [] n = False
search (x:xs) n = if x/=n then search xs n
                  else True   
  
intersT:: [Int]->[Int]->[Int]->[Int]
intersT [] lis2 acc = acc
intersT (x:xs) lis2 acc = if search lis2 x then intersT xs lis2 (x:acc)
                          else intersT xs lis2 acc
                          
intersW::[Int]->[Int]->[Int]
intersW (x:xs) lis2 = inters (x:xs) lis2 acc where
    acc=[]
    inters [] lis2 acc = acc
    inters (x:xs) lis2 acc = if search lis2 x then inters xs lis2 (x:acc)
                             else inters xs lis2 acc
 
prodS:: [Int]->Int
prodS []=1
prodS (x:xs) = x*prodS xs     
                       
prodM::[[Int]]->Int->Int
prodM [] acc = acc
prodM (x:xs) acc = prodM xs (acc*(prodS x))


data Tree2 a=Empty|Branch a (Tree2 a)(Tree2 a) deriving Show

flatten::Tree2 Int->[Int]
flatten Empty = []
flatten (Branch x l r) = [x]
                         ++ flatten l
                         ++ flatten r
