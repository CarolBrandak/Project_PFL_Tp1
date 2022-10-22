module Test where

import Polinomio
import T1

addTest1 :: IO ()
addTest1 = do putStrLn $ "(" ++ convert p1 0 ++ ") + (" ++ convert p2 0 ++ ")"
              putStrLn $ "Expected: " ++ convert [(2,['x'],[2]), (1,['x','y'],[1,1]), (5,['x'],[1]), (4,[],[])] 0 
              putStrLn $ "Result:   " ++ outputAdd p1 p2 ++ "\n"

addTest2 :: IO ()
addTest2 = do putStrLn $ "(" ++ convert p3 0 ++ ") + (" ++ convert p4 0 ++ ")"
              putStrLn $ "Expected: " ++ convert [(1,['x','y','z'], [2,3,4]), (2,['w','z','x','a','e'],[1,1,1,1,1]), (2,['x'],[1]), (31,[],[])] 0
              putStrLn $ "Result:   " ++ outputAdd p3 p4 ++ "\n"


deriveTest1 :: IO ()
deriveTest1 = do putStrLn $ "Derive: " ++ convert p5 0 ++ "from x"
                 putStrLn $ "Expected: " ++ convert [(2,['y','z'], [1,2])] 0
                 putStrLn $ "Result:   " ++ convert (derivePoly p5 'x') 0


deriveTest2 :: IO ()
deriveTest2 = do putStrLn $ "Derive: " ++ convert p5 0 ++ "from y"
                 putStrLn $ "Expected: " ++ convert [(2,['x', 'z'],[1,2]), (-6,['z'],[2])] 0
                 putStrLn $ "Result:   " ++ convert (derivePoly p5 'y') 0


deriveTest3 :: IO ()
deriveTest3 = do putStrLn $ "Derive: " ++ convert p5 0 ++ "from z"
                 putStrLn $ "Expected: " ++ convert [(4,['x','y','z'],[1,1,1]), (-12,['z','y'],[1,1]), (27,['z'],[2])] 0
                 putStrLn $ "Result:   " ++ convert (derivePoly p5 'z') 0