module Polinomio where

type Monomio = (Int, [Char], [Int])

type Poly = [Monomio]

p0 :: Poly
p0 = [(0, ['x', 'y', 'z'], [2, 0, 0]), (2, ['x', 'y', 'z'], [0, 1, 0]), (5, ['x', 'y', 'z'], [0, 0, 1]), (1, ['x', 'y', 'z'], [0, 1, 0]), (7, ['x', 'y', 'z'], [0, 2, 0])]

s0 :: String
s0 = "0x^2+2y+5z+y+7y^2"

p1 :: Poly
p1 = [(3, ['x'], [2]), (5, ['x'], [1]), (-6, [], [])]

s1 :: String
s1 = "3x^2+5x-6"

p2 :: Poly
p2 = [(-1, ['x'], [2]), (1, ['x', 'y'], [1, 1]), (10, [], [])]

s2 :: String
s2 = "-x^2+xy+10"

p3 :: Poly
p3 = [(2, ['w', 'z', 'x', 'a', 'e'], [1, 1, 1, 1, 1]), (30, [], [])]

s3 :: String
s3 = "2wzxae+30"

p4 :: Poly
p4 = [(1, ['x', 'y', 'z'], [2, 3, 4]), (2, ['x'], [1]), (1, [], [])]

s4 :: String
s4 = "x^2y^3z^4+2x+1"

p5 :: Poly
p5 = [(2, ['x', 'y', 'z'], [1, 1, 2]), (-6, ['x', 'z', 'y'], [0, 2, 1]), (9, ['x', 'y', 'z'], [0, 0, 3])]

s5 :: String
s5 = "2xyz^2-6z^2y+9z^3"