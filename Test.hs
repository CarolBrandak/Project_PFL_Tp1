module Test where

import Polinomio
import T1

--Testes de normalizar
normalizeTest1 :: IO ()
normalizeTest1 = do
  putStrLn $ "(" ++ convert (remove0 p0) 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(7, ['x', 'y'], [0, 2]), (3, ['x', 'y'], [0, 1]), (5, ['x', 'y', 'z'], [0, 0, 1])] 0
  putStrLn $ "Result:   " ++ outputNormalize p0 ++ "\n"

normalizeTest2 :: IO ()
normalizeTest2 = do
  putStrLn $ "(" ++ convert (remove0 p3) 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(2, ['a', 'e', 'w', 'x', 'z'], [1, 1, 1, 1, 1]), (30, [], [])] 0
  putStrLn $ "Result:   " ++ outputNormalize p3 ++ "\n"

normalizeTest3 :: IO ()
normalizeTest3 = do
  putStrLn $ "(" ++ convert (remove0 p5) 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(9, ['z'], [3]), (2, ['z', 'x', 'y'], [2, 1, 1]), (-6, ['z', 'y'], [2, 1])] 0
  putStrLn $ "Result:   " ++ outputNormalize p5 ++ "\n"

--Testes de adicionar
addTest1 :: IO ()
addTest1 = do
  putStrLn $ "(" ++ convert p1 0 ++ ") + (" ++ convert p2 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(2, ['x'], [2]), (1, ['x', 'y'], [1, 1]), (5, ['x'], [1]), (4, [], [])] 0
  putStrLn $ "Result:   " ++ outputAdd p1 p2 ++ "\n"

addTest2 :: IO ()
addTest2 = do
  putStrLn $ "(" ++ convert p3 0 ++ ") + (" ++ convert p4 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(1, ['z', 'y', 'x'], [4, 3, 2]), (2, ['a', 'e', 'w', 'x', 'z'], [1, 1, 1, 1, 1]), (2, ['x'], [1]), (31, [], [])] 0
  putStrLn $ "Result:   " ++ outputAdd p3 p4 ++ "\n"

addTest3 :: IO ()
addTest3 = do
  putStrLn $ "(" ++ convert p0 0 ++ ") + (" ++ convert p5 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(9,"z",[3]),(7,"y",[2]),(2,"zxy",[2,1,1]),(-6,"zy",[2,1]),(3,"y",[1]),(5,"z",[1])] 0
  putStrLn $ "Result:   " ++ outputAdd p0 p5 ++ "\n"

--Testes de multiplicar

multiTest1 :: IO ()
multiTest1 = do
  putStrLn $ "(" ++ convert p0 0 ++ ") + (" ++ convert p1 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(9, "xy", [2, 1]), (15, "xz", [2, 1]), (21, "xy", [2, 2]), (35, "yx", [2, 1]), (-42, "y", [2]), (15, "xy", [1, 1]), (25, "xz", [1, 1]), (-18, "y", [1]), (-30, "z", [1])] 0
  putStrLn $ "Result:   " ++ outputMulti p0 p1 ++ "\n"

multiTest2 :: IO ()
multiTest2 = do
  putStrLn $ "(" ++ convert p3 0 ++ ") + (" ++ convert p4 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(2,"zxyaew",[5,3,3,1,1,1]),(30,"zyx",[4,3,2]),(4,"xaewz",[2,1,1,1,1]),(2,"aewxz",[1,1,1,1,1]),(60,"x",[1]),(30,"",[])] 0
  putStrLn $ "Result:   " ++ outputMulti p3 p4 ++ "\n"

multiTest3 :: IO ()
multiTest3 = do
  putStrLn $ "(" ++ convert p2 0 ++ ") + (" ++ convert p5 0 ++ ")"
  putStrLn $ "Expected: " ++ convert [(-2,"xzy",[3,2,1]),(-9,"zx",[3,2]),(9,"zxy",[3,1,1]),(90,"z",[3]),(6,"xzy",[2,2,1]),(2,"xyz",[2,2,2]),(-6,"yzx",[2,2,1]),(20,"zxy",[2,1,1]),(-60,"zy",[2,1])] 0
  putStrLn $ "Result:   " ++ outputMulti p2 p5 ++ "\n"

--Testes de derivar
deriveTest1 :: IO ()
deriveTest1 = do
  putStrLn $ "Derive: " ++ convert p5 0 ++ " from x"
  putStrLn $ "Expected: " ++ convert [(2, ['y', 'z'], [1, 2])] 0
  putStrLn $ "Result:   " ++ outputDerive p5 'x'

deriveTest2 :: IO ()
deriveTest2 = do
  putStrLn $ "Derive: " ++ convert p5 0 ++ " from y"
  putStrLn $ "Expected: " ++ convert [(2, ['x', 'z'], [1, 2]), (-6, ['z'], [2])] 0
  putStrLn $ "Result:   " ++ outputDerive p5 'y'

deriveTest3 :: IO ()
deriveTest3 = do
  putStrLn $ "Derive: " ++ convert p5 0 ++ " from z"
  putStrLn $ "Expected: " ++ convert [(4, ['x', 'y', 'z'], [1, 1, 1]), (-12, ['z', 'y'], [1, 1]), (27, ['z'], [2])] 0
  putStrLn $ "Result:   " ++ outputDerive p5 'z'
