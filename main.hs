module Main where

import T1
import Polinomio

main :: IO () --Menu
main = do
  putStrLn "1. Normalizar: "
  putStrLn "2. Adicionar: "
  putStrLn "3. Multiplicar: "
  putStrLn "4. Derivada: "
  nr <- getChar
  if nr == '1'
    then putStrLn $ outputNormalize p1
    else
      if nr == '2'
        then putStrLn $ addPoly p1 p1
        else
          if nr == '4'
            then do
              putStrLn $ convert (convertTupleToPoly (orderVar (remove0 (normalize (derivePoly p1 'y'))))) 0
            else do
              return ()