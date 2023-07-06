import System.IO
import Data.List
import Data.Char
--b
line _ [] = []
line a (x:xs) = (a,x):line (a+1) xs

numLines xs = line 1 xs 

--c 
allNumWords [] = []
allNumWords ((a, x):xs) = zip (repeat a) (words x) ++ allNumWords xs

--d
invTupla [] = []
invTupla ((a,x):xs) = (x,a):invTupla xs
sortLs xs = invTupla(sort(invTupla xs)) 

--e
almalgamate [] = []
almalgamate ((n,w):xs) = ([a | (a,b) <- ((n,w) : xs), b == w], w) : almalgamate ([(a,b) | (a,b) <- xs, b /= w])

--f
aux [] = []
aux (x:xs) = x: aux [a | a <- xs, a /= x]
shorten [] =[]
shorten ((ls,w):xs) = (aux ls, w):shorten xs 

--funcao que da print no texto do main
imprimir [] = putStr "\n"
imprimr ((l,s):xs) = do putStr (show l ++ "-")
                        putStr (s++"\n")
                        imprimir xs 

main = do putStr "Informe nome do arquivo: "
          hFlush stdout
          arq <- getLine 
          a <- readFile arq
          let n = shorten(almalgamate(sortLs(allNumWords(numLines (lines (map toLower a))))))
          imprimir n
          
          
          
          
          
          
