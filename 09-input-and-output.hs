{-# LANGUAGE LambdaCase #-}
{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
import Data.List (sort)
import System.Environment (getArgs)
import System.Random (StdGen, mkStdGen, randomR)
  
-- main = do
--   args <- getArgs
--   case args of
--     "-n":xs -> putStr $ unwords xs
--     xs -> putStrLn $ unwords xs

main = getArgs >>= \case
  "-n":xs -> putStr $ unwords xs
  xs -> putStrLn $ unwords xs

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = sort [n0, n1, n2, n3, n4, n5]
  where
    r = randomR (1, 49)
    (n0, g1) = r gen
    (n1, g2) = r g1
    (n2, g3) = r g2
    (n3, g4) = r g3
    (n4, g5) = r g4
    (n5, _)  = r g5
