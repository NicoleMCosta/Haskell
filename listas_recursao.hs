tam [] = 0
tam (x : xs) = tam xs + 1

somatorio [] = 0
somatorio (x:xs) = somatorio xs + x

soma _ [] = []
soma [] _ = []
soma (x : xs) (y : ys) = x + y : soma xs ys

maior [x] = x
maior (x:xs) = if x > m then x else m
 where  m = maior xs
 
pertence a [] = False
pertence a (x : xs) = if (a == x) then True else pertence a xs 

inter [] ys = []
inter (x:xs) ys = if pertence x ys then x:inter xs ys else inter xs ys 

concatenar [] ys = ys
concatenar (x:xs) ys = x:concatenar xs ys

ins a [] = [a]
ins a l@(x:xs)  | a==x = l
                | a < x = a:l
                | otherwise = x:ins a xs
				
nprimeiros n [] = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

inv [] = []  
inv (x:xs) = inv xs ++ [x]  
