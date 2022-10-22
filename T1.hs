import Data.Char
import Data.List
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

compFirst :: Monomio -> Monomio -> Ordering --Compara os monomios
compFirst (_, _, x : _) (_, _, y : _)
  | x > y = LT
  | otherwise = GT

orderPoly :: Poly -> Poly --Ordena o polinomio
orderPoly l = sortBy compFirst (convertTupleToPoly (orderExp l))

variable :: (Char, Int) -> (Char, Int) -> Ordering --Orderna as variaveis
variable (c1, n1) (c2, n2)
  | c1 < c2 = LT
  | otherwise = GT

orderVar :: Poly -> [(Int, ([Char], [Int]))] --Orderna o polinomio por variaveis
orderVar [] = []
orderVar ((a, lc, ln) : xs) = (a, unzip (sortBy variable (zip lc ln))) : orderVar xs

convertTupleToPoly :: [(Int, ([Char], [Int]))] -> Poly --Converte a forma que esta as ordenaçoes para Poly normal
convertTupleToPoly [] = []
convertTupleToPoly ((a, (lc, ln)) : xs) = (a, lc, ln) : convertTupleToPoly xs

expo :: (Char, Int) -> (Char, Int) -> Ordering --Orderna os expoente
expo (c1, n1) (c2, n2)
  | n1 > n2 = LT
  | otherwise = GT

orderExp :: Poly -> [(Int, ([Char], [Int]))] --Orderna o polinomio por expoentes
orderExp [] = []
orderExp ((a, lc, ln) : xs) = (a, unzip (sortBy expo (zip lc ln))) : orderExp xs

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
  | x == 1 = " + " ++ recursiveShow (zip l1 l2) ++ convert xs (cnt + 1)
  | x > 1 = " + " ++ show x ++ recursiveShow (zip l1 l2) ++ convert xs (cnt + 1)
  | x == 0 = convert xs (cnt + 1)
  | x == -1 = " - " ++ recursiveShow (zip l1 l2) ++ convert xs (cnt + 1)
  | otherwise = " " ++ show x ++ recursiveShow (zip l1 l2) ++ convert xs (cnt + 1)

simplifyMonomial :: Monomio -> Poly -> Monomio --Junta todos os monomios iguals
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

addPoly :: Poly -> Poly -> Poly --Adiciona os 2 polinomios
addPoly poly1 poly2 = convertTupleToPoly (orderVar poly1) ++ convertTupleToPoly (orderVar poly2)

multiPoly :: Poly -> Poly -> Poly --Multiplicar os 2 polinomios
multiPoly poly1 poly2 = convertTupleToPoly (orderVar (convertTupleToPoly (multiplyMono (generatePoly (normalize poly1) (normalize poly2)))))

generatePoly :: Poly -> Poly -> [(Monomio, Monomio)] --Junta Monomios com Monomios
generatePoly p1 p2 = [(a, b) | a <- p1, b <- p2]

multiplyMono :: [(Monomio, Monomio)] -> [(Int, ([Char], [Int]))] --Percorre a lista de pares de monomios e junta-os num monomio
multiplyMono [] = []
multiplyMono ((m1, m2) : xs) = joinMono m1 m2 : multiplyMono xs

joinMono :: Monomio -> Monomio -> (Int, ([Char], [Int])) --Junta as listas de variaveis e expoentes dos monomios e multiplica coeficiente
joinMono (a, lc1, ln1) (b, lc2, ln2) = (a * b, unzip (joinVars (zip lc1 ln1 ++ zip lc2 ln2)))

joinVars :: [(Char, Int)] -> [(Char, Int)] --Simplifica as variaveis
joinVars [] = []
joinVars (x : xs) = simplifyVar x xs : joinVars a
  where
    a = deleteEqualVar (simplifyVar x xs) xs

simplifyVar :: (Char, Int) -> [(Char, Int)] -> (Char, Int) --Junta todas as Variaveis iguais
simplifyVar a [] = a
simplifyVar (c1, n1) ((c2, n2) : xs)
  | c1 == c2 = simplifyVar (c1, n1 + n2) xs
  | otherwise = simplifyVar (c1, n1) xs

deleteEqualVar :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)] --Apaga da lista pares com variavel igual à variavel do par
deleteEqualVar a [] = []
deleteEqualVar (c1, n1) ((c2, n2) : xs)
  | c1 == c2 = deleteEqualVar (c1, n1) xs
  | otherwise = (c2, n2) : deleteEqualVar (c1, n1) xs

derivePoly :: Poly -> Char -> Poly --Deriva um polinomio
derivePoly [] _ = []
derivePoly (x:xs) y 
  | getCoef (deriveMono x y) == 0 = derivePoly xs y
  | otherwise = (deriveMono x y) : derivePoly xs y

getCoef :: Monomio -> Int
getCoef (x,_,_) = x

deriveMono :: Monomio -> Char -> Monomio --Deriva um monomio
deriveMono (a, lc, ln) y = (deriveCoef a y lc ln, lc, zipWith (deriveDegree y) lc ln)

deriveCoef :: Int -> Char -> [Char] -> [Int] -> Int -- Altera o coeficiente de um monomio
deriveCoef x y lc ln = x * sum (zipWith (findDegree y) lc ln)

findDegree :: Char -> Char -> Int -> Int --Encontra o grau de uma certa variavel num monomio
findDegree c x y
  | c == x = y
  | otherwise = 0

deriveDegree :: Char -> Char -> Int -> Int --Altera o grau para derivar
deriveDegree x y c
  | x == y = c -1
  | otherwise = c

outputNormalize :: Poly -> String --Output de normalizar
outputNormalize l = convert (orderPoly (normalize (convertTupleToPoly (orderVar (remove0 l))))) 0

outputAdd :: Poly -> Poly -> String --Output de adicionar polinomios
outputAdd poly1 poly2 = convert (orderPoly (remove0 (normalize ( addPoly poly1 poly2)))) 0

outputMulti :: Poly -> Poly -> String --Output de multiplicar polinomios
outputMulti poly1 poly2 = convert (orderPoly (remove0 (normalize (multiPoly poly1 poly2)))) 0

outputDerive :: Poly -> Char -> String --Output de derivar 1 polinomio conforme a variavel
outputDerive l c = convert (convertTupleToPoly (orderVar (remove0 (normalize (derivePoly l c))))) 0

l :: [(Int, [Char], [Int])]
l = [(2, ['x', 'y', 'z'], [0, 1, 2]), (-6, ['x', 'z', 'y'], [0, 2, 1]), (4, ['x', 'y', 'z'], [0, 0, 3]), (5, ['x', 'y', 'z'], [3, 0, 0])]

--[(0, ['x','y','z'], [2,0,0]), (2, ['x','y','z'], [0,1,0]), (5, ['x','y','z'], [0, 0, 1]), (1, ['x','y','z'], [0, 1, 0]), (7, ['x','y','z'], [0, 2, 0]), (2, ['x','y','z'], [1, 0, 0])]
--[(2, ['x','y','z'], [0,1,2]), (-6, ['x','y','z'], [0,1,2]), (4, ['x','y','z'], [0, 0, 3]), (5, ['x','y','z'], [0, 0, 3])]
--[(0, ['x','y','z'], [2,0,0]), (2, ['x','y','z'], [0,1,0]), (5, ['x','y','z'], [0, 0, 1]), (1, ['x','y','z'], [0, 1, 0]), (7, ['x','y','z'], [0, 2, 0])]

main :: IO () --Menu
main = do
  putStrLn "1. Normalizar: "
  putStrLn "2. Adicionar: "
  putStrLn "3. Multiplicar: "
  putStrLn "4. Derivada: "
  nr <- getChar
  if nr == '1'
    then do
      putStrLn $ outputNormalize l
    else
      if nr == '2'
        then do
          putStrLn $ outputAdd l l
        else
          if nr == '3'
            then do
              putStrLn $ outputMulti l l 
            else
              if nr == '4'
                then do
                  c <- getChar
                  putStrLn $ outputDerive l c
                else do
                  return ()