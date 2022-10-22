module Main where

import T1
import Polinomio

l :: [(Int, [Char], [Int])]
l = [(2, ['x', 'x', 'x'], [1, 1, 2]), (-6, ['x', 'z', 'y'], [0, 2, 1]), (4, ['x', 'y', 'z'], [0, 0, 3]), (5, ['x', 'y', 'z'], [3, 0, 0])]

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