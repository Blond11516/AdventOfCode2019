import System.IO

main = do
    withFile "inputs/1.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStrLn $ process (lines contents))

process :: [String] -> String
process = show . sum . map (\mass -> calculateFuel (read mass))

calculateFuel :: Int -> Int
calculateFuel mass = subtract 2 $ div mass 3
