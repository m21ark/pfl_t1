-- Main Project File
import System.IO -- to use output flush 
import Prop_tests -- for QuickCheck Property Based Testing
import Parser

-- Based on:
-- https://oliverbalfour.github.io/haskell/2020/08/09/parsing-arithmetic-with-monads.html
-- https://hackage.haskell.org/package/computational-algebra-0.0.1.1/docs/Algebra-Ring-Polynomial-Parser.html

-- Finds the "!" operator to use replace it with the previous user input  
find_prev :: String -> String -> String
find_prev "" _ = ""
find_prev (x:xs) prev =  if x == '!' then ("(" ++ prev ++")") ++ find_prev xs prev  else [x] ++ find_prev xs prev

-- Main interactive function
main_ :: String -> IO ()
main_ prev = do  
    putStr "> "
    hFlush stdout
    line <- getLine 
    if line /= "exit"  then 
            let previous_l = find_prev line prev in putStrLn (unlines (map repl (lines (previous_l)))) >> (main_ previous_l)
    else return ()

-- For QuickCheck Property Based Testing
main_test :: IO Bool
main_test = check -- used only when doing property based testing

-- Main code call
main :: IO ()
main = main_ "0"

