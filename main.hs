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