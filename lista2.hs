--Lista de Exercícios Moodle 2
import Data.Char

--1
pertence a [] = False
pertence a (x: xs) = if (a == x) then True else pertence a xs

--2
inter [] ys = []
inter (x:xs) ys = if pertence x ys then x:inter xs ys else inter xs ys

--3
inv [] = []
inv (x:xs) = inv xs ++ [x]

--4
nprimeiros n [] = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

nUltimos n (x:xs) = inv (nprimeiros n (inv xs))

--5
soma2 _ [] = []
soma2 [] _ = []
soma2 (x: xs) (y: ys) = (x + y): soma2 xs ys

--6
pot2 n = inv(pot2' n)
pot2' 0 = [1]
pot2' n = 2^n : pot2' (n-1)

--7
intercalacao xs [] = xs--se ys for vazio, retorna xs
intercalacao [] ys = ys
intercalacao (x:xs) (y:ys) = if x < y then x:intercalacao xs (y:ys) else x:intercalacao xs ys

--8
menor [x] = x
menor (x:xs) = if x < m then x else m
              where m = menor xs

--9
removerElem a [] = []
removerElem a (x:xs) = if a == x then xs else x:removerElem a xs

--10
ordenar [] = []
ordenar (x:xs) = m : ordenar (removerElem m (x:xs))
 where m = menor (x:xs)
  
--11
ins a [] = [a]
--ins a (x:xs) = if pertence a (x:xs) then ordenar(removerElem a (x:xs)) else ordenar(x:ins a xs)
ins a (x:xs) = if pertence a (x:xs) then xs else ordenar (a:(x:xs))

--12
enesimo n [] = []
enesimo n (x:xs) = removerElem (n-1) (nprimeiros n (x:xs))

--13
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

--14
--notação posicional: valor associado a digitos e caracteres, correspondetes de alguma forma
numString a = inv (numString' a)
numString' 0 = []
numString' a = chr((rem a 10) + 48):numString' (div a 10)

--15
stringNum [] = 0
stringNum (x:xs) = (((ord x) - 48) * 10^length xs) + stringNum xs 

--16
bin2int [] = 0
bin2int (x:xs) = (((ord x) - 48) * 2^length xs) + bin2int xs

--17
int2bin 0 = []
int2bin a = inv(chr((rem a 2) + 48):int2bin (div a 2))

--18
minusculas [] = []
minusculas (x:xs) = toLower(x):minusculas(xs)
