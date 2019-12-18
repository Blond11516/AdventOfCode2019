import System.IO
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

main = do
    withFile "inputs/2.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let program = replaceValues $ splitOn "," contents
        putStrLn $ process program)

replaceValues :: [String] -> [String]
replaceValues (a:_:_:xs) = a:"12":"2":xs

process :: [String] -> String
process codes = show . Maybe.fromJust . Map.lookup 0 $ execute positionMap 0
    where
        positionMap = Map.fromList . zip [0..] $ map read codes

execute :: Map.Map Int Int -> Int -> Map.Map Int Int
execute map ic
    | opcode == 99 = map
    | opcode == 1 = executeOp (+) map ic
    | opcode == 2 = executeOp (*) map ic
    | otherwise = error "Unknown opcode"
    where
        opcode = Maybe.fromJust $ Map.lookup ic map

executeOp :: (Int -> Int -> Int) -> Map.Map Int Int -> Int -> Map.Map Int Int
executeOp fun map ic = execute (Map.insert target result map) $ ic + 4
    where
        target = Maybe.fromJust $ Map.lookup (ic + 3) map
        result = fun arg1Val arg2Val
        arg1Index = Maybe.fromJust $ Map.lookup (ic + 1) map
        arg1Val = Maybe.fromJust $ Map.lookup arg1Index map
        arg2Index = Maybe.fromJust $ Map.lookup (ic + 2) map
        arg2Val = Maybe.fromJust $ Map.lookup arg2Index map