import System.IO

main = do
    withFile "inputs/1.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStrLn $ process (lines contents))

process :: [String] -> String
process = show . sum . map (\mass -> calculateFuel (read mass))

calculateFuel :: Int -> Int
calculateFuel mass | mass <= 0 = 0
                   | otherwise = correctedFuel + (calculateFuel correctedFuel)
    where
        fuel = subtract 2 $ div mass 3
        correctedFuel = if (fuel < 0) then 0 else fuel