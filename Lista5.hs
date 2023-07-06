import System.IO 
import Data.String 
import Data.Char 
  
data Tree = Node String [Int] Tree Tree | Leaf deriving Show 

--a  
line _ [] = []   
line a (x:xs) = (a,x):line (a+1) xs   
numLines xs = line 1 xs    

--b  
allNumWords [] = []   
allNumWords ((a, x):xs) = zip (repeat a) (words x) ++ allNumWords xs   

--d  
insOrd e [] = e:[]
insOrd e (x:xs) |e == x = x:xs 
                |e < x = e:x:xs
                |otherwise  = x:insOrd e xs
                        
--e 
ins p l Leaf = Node p [l] Leaf Leaf 
ins p l (Node x ls esq dir) |x == p = Node x (insOrd l ls) esq dir  
                            |p < x = Node x ls (ins p l esq) dir 
                            |otherwise  = Node x ls esq (ins p l dir) 

--f
mIndexTree [] tree = tree    
mIndexTree ((l,p):xs) tree = mIndexTree xs (ins p l tree) 

imprimir Leaf = return ()
imprimir (Node x ls esq dir) = do imprimir esq
                                  putStrLn (x ++ " - " ++ show ls)
                                  imprimir dir 

main = do putStr "texto: "
          hFlush stdout
          arq <- getLine
          a <- readFile arq
          let n = mIndexTree (allNumWords(numLines(lines(map toLower a)))) Leaf 
          imprimir n

