import System.IO
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

main = do
    withFile "inputs/2.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let program = splitOn "," contents
        putStrLn $ process program)

valueCombinations = [(x,y) | x <- [0..99], y <- [0..99]]

findValues :: Map.Map Int Int -> [(Int, Int)] -> (Int, Int)
findValues memory (testValues:otherValues) =
    if (return == 19690720)
    then testValues
    else findValues memory otherValues
    where
        return = Maybe.fromJust . Map.lookup 0 $ execute testMemory 0
        testMemory = replaceValues memory testValues

replaceValues :: Map.Map Int Int -> (Int, Int) -> Map.Map Int Int
replaceValues memory (noun, verb) = Map.insert 2 verb $ Map.insert 1 noun memory

process :: [String] -> String
process codes = show . (verb+) $ 100 * noun
    where
        memory = Map.fromList . zip [0..] $ map read codes
        (noun, verb) = findValues memory valueCombinations

execute :: Map.Map Int Int -> Int -> Map.Map Int Int
execute memory ic
    | opcode == 99 = memory
    | opcode == 1 = executeOp (+) memory ic
    | opcode == 2 = executeOp (*) memory ic
    | otherwise = error "Unknown opcode"
    where
        opcode = Maybe.fromJust $ Map.lookup ic memory

executeOp :: (Int -> Int -> Int) -> Map.Map Int Int -> Int -> Map.Map Int Int
executeOp fun memory ic = execute (Map.insert target result memory) $ ic + 4
    where
        target = Maybe.fromJust $ Map.lookup (ic + 3) memory
        result = fun arg1Val arg2Val
        arg1Index = Maybe.fromJust $ Map.lookup (ic + 1) memory
        arg1Val = Maybe.fromJust $ Map.lookup arg1Index memory
        arg2Index = Maybe.fromJust $ Map.lookup (ic + 2) memory
        arg2Val = Maybe.fromJust $ Map.lookup arg2Index memory