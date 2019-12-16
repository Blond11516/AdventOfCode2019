main = print . readInput "inputs/1.txt"

readInput :: FilePath -> String
readInput path = do
    contents <- readFile path
    let inputs = words $ contents
    return inputs