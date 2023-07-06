-------------SEMESTRE 1 DE HASKELL
import Data.Char --funções toLower, toUpper, chr() e ord()

--compilador: gera arquivo executável
--interpretador: executa código fonte
--LER TIPAGEM

--nome função ::()

--FÓRMULAS BÁSICAS

--maior a b = if a > b then a else b
--maior3 a b c = if a > b && a > c then a else if b > c then b else c
--maior3' a b c = maior (maior a b) c
--maior4 a b c d = maior (maior3 a b c) d
ehtriangulo' a b c = if (a + b < c) || (a + c < b) || (b + c < a) then False else True
ehtriangulo a b c = a + b < c && a + c < b && b + c < a

f x = x + 1
g x y = 2 * x + y

negacao b = if b == True then False else True
negacao' b = if b then False else True
neg False = True
neg True = False

oou a b = if a == True then if b == True then True else False else False
ou' a b = if a == False && b == False then False else True
ou'' a b = if a == False then if b == False then False else True else True

ou True True = True
ou True False = True 
ou False True = True 
ou False False = False

ouu False False = False
ouu _ _ = True

e True True = True
e _ _ = False

par a = if rem a 2 == 0 then "par" else "impar"
par' a = rem a 2 == 0

fat 0 = 1
fat n = n * fat(n-1)
pot b 0 = 1
pot b e = b * pot b (e-1)
mdc a b = if a == b then a else if a > b then mdc(a-b)b else mdc a (b-a)


--------------------------

--LISTAS E RECURSIVAS

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

-- a:x:xs concatena a com (x:xs) ||  x : função, concatena x com resultado da função, como lista
--ins a (x:xs) = if pertence a (x:xs) then x:xs else if a < x then a:x:xs  else x: ins a xs
--sintaxe para da nome ao dados:
ins a [] = [a]
--ins a l@(x:xs) = if pertence a l then x:xs else if a < x then a:l  else x: ins a xs
ins a l@(x:xs)  | a==x = l
                | a < x = a:l
                | otherwise = x:ins a xs
		
		
-- (n - 1) limita a operação, fazendo com que seja feita apenas até o valor n
nprimeiros n [] = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs


-- operador concatenador que une ao fim da lista
inv [] = []  
inv (x:xs) = inv xs ++ [x]  




--------------------------


--COMPREENSÃO DE LISTAS
--NOTAÇÃO DE CONJUNTOS EM HASKELL

-- sinais:
-- .. == until
-- <- == "vem da lista"
-- , == separa argumentos ao ser usado mais de um em uma unica notação de conjunto

--funções/ operações:
-- [x | x <- [0..]] == x tal que x de 0 a infinito (.. indica until)
--take n [notação de conjunto ] == pega n elementos pertencentes a lista de conjunto
-- [x + y | x <-[0..n, y <- [0..5]]] == conta de elementos de listas, separadas por vírgula


par'' x = rem x 2 == 0 -- resto de x / 2 == 0, números divisiveis por 2
-- [x | x <- [0..], par x] no prompt, função par pre-definida no editor de texto.

remover _ [] = []
remover a (x:xs) = if a == x then remover a xs else x:remover a xs

remover' a xs = [x| x <- xs, x /= a]



----------------------------

--prova 1 até aqui
--TUPLAS

primeiro (x,y) = x
segundo (x,y) = y
soma' x y = x + y
--no terminal: "soma :: Num a => a -> a -> a"
soma'' (x,y) = x + y
--no terminal: "soma' :: Num a => (a, a) -> a" *Num é a condicional que o a deve ser numérico
-- a diferença dos dois é que ao receber uma dupla, podemos fazer a aplicação parcial de uma função

--t ((,)) **construtor de tuplas
--((,)) :: a -> b -> (a, b)

juntar [] _  = []
juntar _ []  = []
juntar (x:xs) (y:ys) = (x,y):juntar xs ys

procurar e [] = []
procurar e ((n,t): ls) = if e == n then t else procurar e ls

tab' _ [] = []
tab' x (y:ys) = (x, y, x * y) : tab' x ys
tab [] _ = []
tab (x:xs) (y:ys) = tab' x (y:ys) ++ tab xs (y:ys)

tab'' xs ys = [(x,y,x*y)| x <- xs, y <- ys] --mesma função, com sintaxe de compreensão de listas


--snd (_, y) = y --função que retorna segundo valor
--fst (x, _) = x --função que retorna primeiro valor
separar [] = ([],[])
separar ((x,y):xys) = (x: xs, y: ys)
 where (xs,ys) = separar xys

juntar' [] _ = []
juntar' _ [] = []
juntar' (x:xs) (y:ys) = (x,y): juntar' xs ys
-- a função juntar' resultaria em uma entrada de duas listas q retornariam duplas do primeiro elemento de cada lista, recursivamente

remover'' n [] = []
remover'' n (x:xs) = if n == x then remover'' n xs else x: remover'' n xs

removaDup [] =[]
removaDup (x:xs) = x:removaDup (remover x xs)

separar' [] = ([],[])
separar' (x:xs) = if x /= '\n' then (x:ys,zs) else ([],xs)
 where (ys,zs) = separar' xs




---------------------------------

--prova 2 abaixo
--LAMBDA

-- VALORES DE PRIMEIRA CLASSE

--inc :: Int -> Int
inc x = x + 1 	

--aplica2:: (a->a) -> a -> a
aplica2 f x = f(f x)

-- no terminal > chamar aplica2 inc x



--NOTAÇÃO LAMBDA EM FUNÇÃO HASKELL: 
--equivalente a /lambdaf/lambdax.f(f x)
--- \f x -> \x -> f (f x)
--- \f x -> f (f x)



--no terminal > aplica2 (\x -> x + 1) a, chamo sem necessitar declarar função anterior

test1 = (\x y -> x + y) 10
--test1 = \y -> 10 + y, 10 assume tds os valores de x pelo \x inicial
--no terminal > test1 (numero qualquer) e esse valor irá entrar para ser trabalhado com o \y


(mais) = \x y -> x + y --função chamada (+) que faz o trabalho que está sendo feito em test1
--no terminal> poderia fazer aplica2 (1+) x que resultaria no mesmo resultado

(divs) = \x y -> x/y --função chamada (/) que executa divisão usando notação lambda
--no terminal > aplica2 (16/) 2
-- > 8

myFlip f x y = f y x --inverte parametros de uma função chamda em f


----------------------------------------------------------------------------
--ARQ: Exemplo de Cálculo Lambda, moodle, pelo Prof. Crisftiano(PFN)        |
zero::(a -> a) -> a -> a
zero = \s z -> z
um::(a -> a) -> a -> a
um = \s z -> s z
dois = \s z -> s (s z)
tres = \s z -> s (s (s z))
quatro = \s z -> s (s (s (s z)))


suc = \w y x -> y (w y x)
prede = \n f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u)
add = \x y w u -> x w (y w u)
mul = \x y w u -> x (y w) u

v1 = \v f -> v
f1 = \v f -> f

e1 = \x y -> x y (\u v -> v)
ou1 = \x y -> x (\u v -> u) y
nao1 = \x -> x (\ u v -> v) (\a b -> a)

ehZero = \n -> n (\d -> f) v1

teste = \n -> (ehZero n) um dois                                       -- |
---------------------------------------------------------------------------




----------------------------------


--FUNÇÕES DE ORDEM SUPERIOR

--Função que generealiza um parâmetro


--map'::(a -> b) -> [a] -> [b]
--map' f xs =  [f x| x <- xs]
map' _ [] = []
map' f (x:xs) = f x: map' f xs --no lugar de f, em () podemos aplicar uma equação qaulquer que será recursiva em todos os elementos de uma lista

maiuscula xs = map' toUpper xs
maiscula' = map' toUpper

--filter é uma função de ordem superior já existente em haskell
--filter':: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)| f x = x:filter' f xs
                | otherwise = filter' f xs
--filter (<10) [...] == lambdaX.X < 10   
--filter pares em uma lista:  filter' (\x -> rem x 2 == 0) [....]
--filter de função pares == filter' p xs = [x | x <- xs, p x]


particao p xs = (filter' p xs, filter' (\x -> not (p x)) xs)
particao' p xs = (filter' p xs, filter' (not.p) xs)
--(.) = \f\g\x.f (g x) == \x -> not (p x) com os parametros passados "not" e "p"


--somatorio(x:xs) = x + somatorio xs

produtorio [ ] = 1
produtorio (x:xs) = x * produtorio xs

--foldr’ :: (a->a->a)-> a ->[a] -> a
--foldr' função que pode ser usada como somatorio ou produtorio
foldr' f b [] = b            --b é a parada
foldr' f b (x:xs) = f x (foldr' f b xs)
--no terminal : >foldr’ (+) 0 [1,2,3]

map'' f ys = foldr'(\x xs -> f x:xs) [] ys 

--funções do haskell:
--lines : lê uma string e separa \n em uma lista de strings
--putStr: lê uma string e adiciona quebra de linha em cada \n
--length(lines "") : devolve o numero de linhas na string

-- ^^funções a baixo pertencem a Atividade 4
putStr' _ [] = []
putStr' a (x:xs) = if x == '\n' then (a,xs):putStr' (a+1) xs else putStr' a xs
 
numLines xs = putStr' 1 xs

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys
            
numLines' xs = zip [1..] xs

-----------------------------------------

--MONADAS: simulação de "efeito colateral" (alteração de dados na memória)

--transparencia referencial: linguagens funcionais puras garantem que os mesmo valores retornem os mesmos resultados

--Buffer: espaço de memória para ser utilizado quando necessário
--UNITY: dado que não se altera/ valor unitário

--return:: a -> Ma
--(>>):: Ma -> Mb -> Mb ("em sequencia fará")
--(>>=) :: Ma -> a -> Mb -> Mb ("sequente resultado")

--Funções combinadas obrigatóriamente devem retornar o mesmo monada, repare que ambas abaixo retornam o monada IO:
--putStr:: String -> IO()
--getLine:: IO String

main = putStr "QUAL SEU NOME? \n" >> getLine >>= (\nome -> putStr ("Alo "++ nome++"\n"))

--forma de facilitação sintática por haskell
main' = do putStr "QUAL SEU NOME?\n"
           nome <- getLine
           putStr ("Alo\t"++ nome ++ "\n")
          

--na função abaixo, usa uma variável que receba a função fatorial (não monoda)
--show :: a -> String
--read :: String -> a

main'' = do putStr "NUMERO: "
            num <- getLine
            let n = fat(read num)
            putStr ("RESULTADO FATORIAL: "++ show n ++ "\t"++ show num++ "\n" )

--no terminal: > ghc [nome arquivo].hs cria executável
-- > ./[nome arquivo]

    

