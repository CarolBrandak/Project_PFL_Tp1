import Data.List
import Data.Char
import System.IO

type Monomio = (Int, [Char], [Int])

type Poly = [Monomio]

--[(2, ['x','y','z'], [0,1,2]), (-6, ['x','y','z'], [0,1,2])] 2*y*z^2 - 6*y*z^2
--[(2, ['x','y','z'], [0,1,2]), (-6, ['x','y','z'], [0,1,2]), (4, ['x','y','z'], [1, 0, 3]), (5, ['x','y','z'], [1, 0, 3])]
--[(0, ['x','y','z'], [2,0,0]), (2, ['x','y','z'], [0,1,0]), (5, ['x','y','z'], [0, 0, 1]), (1, ['x','y','z'], [0, 1, 0]), (7, ['x','y','z'], [0, 2, 0])]

remove0 :: Poly -> Poly --Remove os zeros do polinomio
remove0 [] = []
remove0 ((x, l1, l2) : xs)
  | x == 0 = xs
  | otherwise = (x, l1, l2) : remove0 xs

compFirst :: Monomio -> Monomio -> Ordering --Compara para depois ordenar
compFirst (_, _, x : _) (_, _, y : _)
  | x > y = LT
  | otherwise = GT

variable :: (Char, Int) -> (Char, Int) -> Ordering
variable (c1,n1) (c2,n2)
  | c1 < c2 = LT  
  | otherwise = GT

orderVar :: Poly -> [(Int, ([Char], [Int]))]  
orderVar [] = []
orderVar ((a, lc, ln):xs) = (a, unzip (sortBy variable (zip lc ln))) : orderVar xs

convertTupleToPoly :: [(Int, ([Char], [Int]))] -> Poly
convertTupleToPoly [] = []
convertTupleToPoly ((a, (lc, ln)):xs) = (a, lc, ln) : convertTupleToPoly xs


orderPoly :: Poly -> Poly --Ordena o polinomio
orderPoly = sortBy compFirst

--sortMono :: Monomio -> Monomio
--sortMono (x:xs) = sortBy x : sortMono 

recursiveShow :: [(Char, Int)] -> String --Escreve em string a parte das variaveis do polinomio
recursiveShow [] = ""
recursiveShow ((a, 0) : xs) = recursiveShow xs
recursiveShow ((a, 1) : xs) = a : recursiveShow xs
recursiveShow ((a, b) : xs)
  | null xs = [a] ++ "^" ++ show b
  | otherwise = [a] ++ "^" ++ show b ++ recursiveShow xs

convert :: Poly -> Int -> String --Converte o polinomio em string
convert [] _ = ""
convert ((x, l1, l2) : xs) 0 
  | x == 0 = convert xs 1
  | otherwise = show x ++ recursiveShow (zip l1 l2) ++ convert xs 1
convert ((x, l1, l2) : xs) cnt
  | x > 0 = " + " ++ show x ++ recursiveShow (zip l1 l2) ++ convert xs (cnt + 1)
  | x == 0 = convert xs (cnt + 1)
  | otherwise = " " ++ show x ++ recursiveShow (zip l1 l2) ++ convert xs (cnt + 1)

simplifyMonomial :: Monomio -> Poly -> Monomio --Junta todos os monomios iguals  (nao considera [x,y,z] e [z,x,y] iguais)
simplifyMonomial a [] = a
simplifyMonomial (a, lc1, ln1) ((b, lc2, ln2) : xs2)
  | lc1 == lc2 && ln1 == ln2 = simplifyMonomial (a + b, lc1, ln1) xs2
  | otherwise = simplifyMonomial (a, lc1, ln1) xs2

deleteEqualMon :: Monomio -> Poly -> Poly --Apaga os monomios iguais
deleteEqualMon a [] = []
deleteEqualMon (a, lc1, ln1) ((b, lc2, ln2) : xs)
  | lc1 == lc2 && ln1 == ln2 = deleteEqualMon (a, lc1, ln1) xs
  | otherwise = (b, lc2, ln2) : deleteEqualMon (a, lc1, ln1) xs

normalize :: Poly -> Poly --Normaliza o polinomio
normalize [] = []
normalize (x : xs) = simplifyMonomial x xs : normalize a
  where
    a = deleteEqualMon (simplifyMonomial x xs) xs

outputNormalize :: Poly -> String --Converte o polinomio normalizado em string
outputNormalize l = convert (convertTupleToPoly (orderVar (normalize (remove0 l)))) 0

addPoly :: Poly -> Poly -> String --Adiciona os 2 polinomios
addPoly poly1 poly2 = convert (convertTupleToPoly (orderVar  (remove0 (normalize (poly1 ++ poly2))))) 0

generatePoly :: Poly -> Poly -> [(Monomio, Monomio)]
generatePoly p1 p2 = [(a,b) | a <- p1, b <- p2]

--multiplyMono :: [(Monomio, Monomio)] -> Poly
--multiplyMono ((m1, m2):xs) = joinMono m1 m2 : multiplyMono xs 

--joinMono :: Monomio -> Monomio -> Monomio
--joinMono (a, lc1, ln1) (b, lc2, ln2) = () -- to do

derivePoly :: Poly -> Char -> Poly --Deriva um polinomio
derivePoly [] _ = []
derivePoly (x:xs) y = deriveMono x y : derivePoly xs y

deriveMono :: Monomio -> Char -> Monomio --Deriva um monomio
deriveMono (a,lc,ln) y = (deriveCoef a y lc ln, lc, zipWith (deriveDegree y) lc ln)


deriveCoef :: Int -> Char -> [Char] -> [Int] -> Int -- Altera o coeficiente de um monomio
deriveCoef x y lc ln = x * sum(zipWith (findDegree y) lc ln)

findDegree :: Char -> Char -> Int -> Int --Encontra o grau de uma certa variavel num monomio
findDegree c x y
    | c == x = y
    | otherwise = 0


deriveDegree :: Char -> Char -> Int -> Int --Altera o grau para derivar
deriveDegree x y c
    | x == y = c-1
    | otherwise = c







l :: [(Int, [Char], [Int])]
l = [(0, ['x','y','z'], [2,0,0]), (2, ['y','z','x'], [2,1,1]), (5, ['x','y','z'], [0, 0, 1]), (1, ['x','y','z'], [0, 1, 0]), (7, ['x','y','z'], [0, 2, 0]), (2, ['x','y','z'], [1, 0, 0])]

--[(0, ['x','y','z'], [2,0,0]), (2, ['x','y','z'], [0,1,0]), (5, ['x','y','z'], [0, 0, 1]), (1, ['x','y','z'], [0, 1, 0]), (7, ['x','y','z'], [0, 2, 0]), (2, ['x','y','z'], [1, 0, 0])]
--[(2, ['x','y','z'], [0,1,2]), (-6, ['x','y','z'], [0,1,2]), (4, ['x','y','z'], [0, 0, 3]), (5, ['x','y','z'], [0, 0, 3])]
--[(0, ['x','y','z'], [2,0,0]), (2, ['x','y','z'], [0,1,0]), (5, ['x','y','z'], [0, 0, 1]), (1, ['x','y','z'], [0, 1, 0]), (7, ['x','y','z'], [0, 2, 0])]

main :: IO () --Menu
main = do
  putStrLn "1. Normalizar: "
  putStrLn "2. Adicionar: "
  putStrLn "3. Multiplicar: "
  putStrLn "4. Derivada: "
  nr <-getChar 
  if nr == '1' then 
    putStrLn $ outputNormalize l
  else if nr == '2' then
    putStrLn $ addPoly l l

  else if nr == '4' then do
      putStrLn $ convert (convertTupleToPoly(orderVar (remove0 (normalize(derivePoly l 'y'))))) 0
  else do
    return ()