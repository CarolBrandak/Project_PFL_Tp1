--import Data.List.Split

type Monomio = (Int, [Char], [Int])
type Poly = [Monomio]

{-
splitPlus :: String -> [String]
splitPlus = splitOn "+"

splitSub :: [String] -> [[String]]
splitSub l = [x | xs <- l, let x = split (whenElt (<0)) xs]

splitPoly :: String -> [String]
splitPoly l = concat (splitSub (splitPlus l))

-}
--[(2, [x,y,z], [0,1,2]), (-6, [0,1,2]))] 2*y*z^2 - 6*y*z^2

--normalize :: [(Int, [Int])] -> String
--normalize (0,b):hs = 

remove0 :: Poly -> Poly
remove0 [] = []
remove0 ((x,l1,l2):xs)
    | x == 0 = xs
    | otherwise = (x,l1,l2) : remove0 xs

convert :: Poly -> String
convert [] = ""
convert [(x,l1,l2)] = show x ++ "*" ++ []