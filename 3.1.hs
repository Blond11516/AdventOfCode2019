import System.IO
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

data Direction = Up | Down | Left | Right
data Position = Position Int Int deriving (Show, Eq, Ord)
data Movement = Movement Int Direction

main = do
    withFile "inputs/3.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStrLn . show $ process contents)

process :: String -> Int
process input = findNearestIntersectionDistance intersections
    where
        inputMoves = lines input
        parsedMoves = [map parseMovement (splitOn "," moves) | moves <- inputMoves]
        paths = map calculatePath parsedMoves
        pathsWithoutOrigin = map Set.fromList $ map (\path -> filter (\pos -> pos /= (Position 0 0)) path) paths
        intersections = findIntersections (pathsWithoutOrigin !! 0) (pathsWithoutOrigin !! 1)

manhattanDistance :: Position -> Int
manhattanDistance (Position x y) = (abs x) + (abs y)

findIntersections :: Set.Set Position -> Set.Set Position -> [Position]
findIntersections path1 path2 = Set.toList $ Set.filter (\pos -> Set.member pos path2) path1

findNearestIntersectionDistance :: [Position] -> Int
findNearestIntersectionDistance = minimum . map manhattanDistance

parseMovement :: String -> Movement
parseMovement ('U':n) = Movement (read n) Up
parseMovement ('D':n) = Movement (read n) Down
parseMovement ('L':n) = Movement (read n) Main.Left
parseMovement ('R':n) = Movement (read n) Main.Right
parseMovement _ = Movement 0 Up

calculatePath :: [Movement] -> [Position]
calculatePath = foldl applyMovement [(Position 0 0)]

applyMovement :: [Position] -> Movement -> [Position]
applyMovement path (Movement 0 _) = path
applyMovement (currentPos:rest) (Movement n dir) = applyMovement ((applyStep currentPos dir):currentPos:rest) (Movement (n - 1) dir)

applyStep :: Position -> Direction -> Position
applyStep (Position currentX currentY) Up = Position currentX (currentY + 1)
applyStep (Position currentX currentY) Down = Position currentX (currentY - 1)
applyStep (Position currentX currentY) Main.Left = Position (currentX - 1) currentY
applyStep (Position currentX currentY) Main.Right = Position (currentX + 1) currentY
