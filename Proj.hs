-- Main Project File
import System.IO -- to use output flush 
import Prop_tests -- for QuickCheck Property Based Testing
import Parser

-- Based on:
-- https://oliverbalfour.github.io/haskell/2020/08/09/parsing-arithmetic-with-monads.html

-- EU não me basiei no de baixo xD
-- https://hackage.haskell.org/package/computational-algebra-0.0.1.1/docs/Algebra-Ring-Polynomial-Parser.html

find_prev :: String -> String -> String
find_prev "" _ = ""
find_prev (x:xs) prev =  if x == '!' then ("(" ++ prev ++")") ++ find_prev xs prev  else [x] ++ find_prev xs prev

main_ :: String -> IO ()
main_ prev = do  
    putStr "> "
    hFlush stdout
    line <- getLine 
    -- NOTA : O facto de termos varios () no ! pode piorar o desempenho 
    if line /= "exit"  then 
            let previous_l = find_prev line prev in putStrLn (unlines (map repl (lines (previous_l)))) >> (main_ previous_l)
    else return ()


-- For QuickCheck Property Based Testing
main_test :: IO Bool
main_test = check

-- Main code call
main :: IO ()
main = main_test >> main_ "0"
