map' _ [] = []
map' f (x:xs) = f x: map' f xs 

maiuscula xs = map' toUpper xs
maiscula' = map' toUpper

filter' _ [] = []
filter' f (x:xs)| f x = x:filter' f xs
                | otherwise = filter' f xs

particao p xs = (filter' p xs, filter' (\x -> not (p x)) xs)
particao' p xs = (filter' p xs, filter' (not.p) xs)

produtorio [ ] = 1
produtorio (x:xs) = x * produtorio xs

foldr' f b [] = b            --b é a parada
foldr' f b (x:xs) = f x (foldr' f b xs)

map'' f ys = foldr'(\x xs -> f x:xs) [] ys 

putStr' _ [] = []
putStr' a (x:xs) = if x == '\n' then (a,xs):putStr' (a+1) xs else putStr' a xs
 
numLines xs = putStr' 1 xs

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys
            
numLines' xs = zip [1..] xs