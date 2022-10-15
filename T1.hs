type Monomio = (Int, [Char], [Int])
type Poly = [Monomio]

--[(2, ['x','y','z'], [0,1,2]), (-6, ['x','y','z'], ['0','1','2']))] 2*y*z^2 - 6*y*z^2

--normalize :: [(Int, [Int])] -> String
--normalize (0,b):hs = 

simplifyMonomial :: Monomio -> Poly -> Monomio
simplifyMonomial a [] = a
simplifyMonomial (a,lc1,ln1) ((b,lc2,ln2):xs2)
    | lc1 == lc2 && ln1 == ln2 = simplifyMonomial (a+b, lc1, ln1) xs2
    | otherwise = simplifyMonomial (a,lc1,ln1) xs2

remove0 :: Poly -> Poly
remove0 [] = []
remove0 ((x,l1,l2):xs)
    | x == 0 = xs
    | otherwise = (x,l1,l2) : remove0 xs

recursiveShow :: [(Char, Int)] -> String
recursiveShow [] = ""
recursiveShow ((a,0):xs) = recursiveShow xs
recursiveShow ((a,b):xs)
    | null xs = [a] ++ "^" ++ show b
    | otherwise = [a] ++ "^" ++ show b ++ "*" ++ recursiveShow xs


convert :: Poly -> Int -> String
convert [] _ = ""
convert ((x,l1,l2):xs) 0 = show x ++ "*" ++ recursiveShow (zip l1 l2) ++ convert xs 1
convert ((x,l1,l2):xs) cnt
    | x > 0 = " + " ++ show x ++ "*" ++ recursiveShow (zip l1 l2) ++ convert xs (cnt+1)
    | otherwise = " " ++show x ++ "*" ++ recursiveShow (zip l1 l2) ++ convert xs (cnt+1)
