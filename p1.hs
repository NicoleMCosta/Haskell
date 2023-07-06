nprm 0 _ = []
nprm n (x:xs) = x:nprm (n-1)xs

remove n [] = []
remove n (x:xs)= if n == x then xs else x:remove n xs

enesimo n [] = 0
enesimo 0 xs = 0
enesimo n (x:xs) = remove x (remove x (nprm n (x:xs)))

juntar [] _ = []
juntar _ [] = []
--juntar (x:xs) (y:ys) = (x,y):juntar xs ys

juntar xs ys = [(a,b)| a <- xs, b <- ys]

removeD [] = []
removeD (x:xs) = x:removeD(remove x xs)

acker 0 n = n + 1
acker m 0 = acker (m-1) 1
acker m n = acker (m-1) (acker m (n-1))

todos [] = True
todos (x:xs)|x == False = False
            |otherwise = todos xs

pares [] = []
pares (x:xs)|rem x 2 == 0 = x:pares xs 
            |otherwise = pares xs
			
ocorrencias' a [] c = c			
ocorrencias' a (b:bs) c| a == b = ocorrencias' a bs (c+1)
                       |otherwise = ocorrencias' a bs cocorrencias a (x:xs) = ocorrencias' a (x:xs) 0 

inverteDuplas [] = []
inverteDuplas ((a,b):bs) = (b,a):inverteDuplas bs
