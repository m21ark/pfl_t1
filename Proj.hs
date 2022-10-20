-- Main Project File
import System.IO -- to use output flush 
import Prop_tests -- for QuickCheck Property Based Testing
import Parser

-- Based on
-- https://oliverbalfour.github.io/haskell/2020/08/09/parsing-arithmetic-with-monads.html
-- https://hackage.haskell.org/package/computational-algebra-0.0.1.1/docs/Algebra-Ring-Polynomial-Parser.html

find_prev :: String -> String -> String
find_prev "" _ = ""
find_prev (x:xs) prev =  if x == '!' then ("(" ++ prev ++")") ++ find_prev xs prev  else [x] ++ find_prev xs prev

main_ :: String -> IO ()
main_ prev = do  
    putStr "> "
    hFlush stdout
    line <- getLine 
    -- previous_input <- line
    if line /= "exit"  then 
            putStrLn (unlines (map repl (lines (find_prev line prev)))) >> (main_ line)
    else return ()

-- Main code call
main :: IO ()
main = main_ "0"

-- For QuickCheck Property Based Testing
main_test :: IO Bool
main_test = check
