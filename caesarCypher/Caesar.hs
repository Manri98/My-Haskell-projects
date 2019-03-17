--module Main where

import System.Environment (getArgs)

encode :: (Int -> String -> String) -> Int -> FilePath -> FilePath -> IO()
encode f shifts inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (f shifts input)


caesar :: Int -> String -> String
caesar x str
    | x == 0 = str
    | x < 0 = caesar (x+1) (map shiftsBackwards str)
    | x > 0 = caesar (x-1) (map shiftForwards str)

shiftForwards :: Char -> Char
shiftForwards ' ' = ' ' 
shiftForwards '\n' = '\n'
shiftForwards 'z' = 'a'
shiftForwards 'Z' = 'A'
shiftForwards x = succ x


shiftsBackwards :: Char -> Char
shiftsBackwards ' ' = ' '
shiftsBackwards '\n' = '\n'
shiftsBackwards 'a' = 'z'
shiftsBackwards 'A' = 'Z'
shiftsBackwards x = pred x

main :: IO ()
main = do
        args <- getArgs
        case args of
            [input, output, shifts] -> encode caesar shifts input output 
                where shifts = shifts :: Int
            _ -> putStrLn "Error: exactly three arguments needed"
