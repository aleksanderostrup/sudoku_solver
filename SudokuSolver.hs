import Data.List

data SudokuCell = V Int | E
  deriving (Eq)

instance Show SudokuCell where
  show (V i) = show i
  show E = " "

type SudokuRow = [SudokuCell]

type SudokuCol = [SudokuCell]

type SudokuBoard = [SudokuRow]

type Pos = (Int, Int)

easySudoku :: [[SudokuCell]]
easySudoku =
  [ [V 7, V 4, V 5, V 3, V 9, V 6, E, V 2, V 8],
    [E, V 9, E, E, V 8, V 4, E, V 7, V 3],
    [V 8, E, V 2, V 1, E, E, E, V 4, E],
    [E, E, V 1, V 9, V 2, E, E, E, E],
    [E, E, V 9, E, E, V 3, E, V 1, V 5],
    [E, E, E, E, E, E, V 6, V 9, V 2],
    [E, E, E, V 6, E, V 2, V 3, E, E],
    [E, V 5, V 7, E, E, V 1, V 4, V 6, E],
    [E, V 1, V 3, E, E, E, E, E, V 7]
  ]

easySudokuSol :: [[SudokuCell]]
easySudokuSol =
  [ [V 7, V 4, V 5, V 3, V 9, V 6, V 1, V 2, V 8],
    [V 1, V 9, V 6, V 2, V 8, V 4, V 5, V 7, V 3],
    [V 8, V 3, V 2, V 1, V 5, V 7, V 9, V 4, V 6],
    [V 5, V 6, V 1, V 9, V 2, V 8, V 7, V 3, V 4],
    [V 4, V 2, V 9, V 7, V 6, V 3, V 8, V 1, V 5],
    [V 3, V 7, V 8, V 4, V 1, V 5, V 6, V 9, V 2],
    [V 9, V 8, V 4, V 6, V 7, V 2, V 3, V 5, V 1],
    [V 2, V 5, V 7, V 8, V 3, V 1, V 4, V 6, V 9],
    [V 6, V 1, V 3, V 5, V 4, V 9, V 2, V 8, V 7]
  ]

extremeSudoku :: [[SudokuCell]]
extremeSudoku =
  [ [E, E, V 9, E, V 8, V 5, E, E, E],
    [E, V 6, E, E, E, E, E, E, V 9],
    [E, V 7, V 8, E, E, E, E, V 1, V 4],
    [E, E, E, E, E, E, E, E, E],
    [E, E, V 5, E, V 1, V 8, E, E, E],
    [E, E, E, V 7, E, E, V 4, V 8, V 2],
    [E, E, E, E, E, V 7, E, V 4, E],
    [V 2, E, E, V 6, E, V 9, E, E, E],
    [E, V 8, E, E, E, E, E, V 7, E]
  ]

prettyRowStr :: SudokuRow -> String
prettyRowStr = foldl (\acc x -> acc ++ show x ++ "|") "|"

prettyPrint :: [SudokuRow] -> IO ()
prettyPrint = mapM_ (putStrLn . prettyRowStr)

getMissingVals :: [SudokuCell] -> [SudokuCell]
getMissingVals sr = map V [1 .. 9] \\ sr

getRowFromPos :: [a1] -> (a2, Int) -> a1
getRowFromPos b p = b !! snd p

-- takes a position and spans a 3 x 3 box.
-- these fields are flattened to a list
flattenBox :: [[a]] -> (Int, Int) -> [a]
flattenBox b (x, y) =
  let x0 = x - mod x 3 -- clamp to upper left of sub box
      y0 = y - mod y 3 -- clamp to upper left of sub box
      r0 = b !! y0
      r1 = b !! (y0 + 1)
      r2 = b !! (y0 + 2)
      x1 = x0 + 1
      x2 = x0 + 2
   in [r0 !! x0, r0 !! x1, r0 !! x2, r1 !! x0, r1 !! x1, r1 !! x2, r2 !! x0, r2 !! x1, r2 !! x2]

getColFromPos :: [[a]] -> (Int, b) -> [a]
getColFromPos b p =
  let btrans = transpose b
   in btrans !! fst p

hasValue :: [[SudokuCell]] -> (Int, Int) -> Bool
hasValue b p = case (b !! snd p) !! fst p of
  E -> False
  V _ -> True

getMissingFromPos :: [[SudokuCell]] -> (Int, Int) -> [SudokuCell]
getMissingFromPos b p =
  let missRow = getMissingVals $ getRowFromPos b p
      missCol = getMissingVals $ getColFromPos b p
      missBox = getMissingVals $ flattenBox b p
   in intersect missBox $ intersect missRow missCol

modifyList :: a -> Int -> [a] -> [a]
modifyList newVal pos list = take pos list ++ newVal : drop (pos + 1) list

modifyBoard :: [[a]] -> (Int, Int) -> a -> [[a]]
modifyBoard b (x, y) v = modifyList (modifyList v x (b !! y)) y b

nextPosFrom :: (Eq a, Eq b, Num a, Num b) => (a, b) -> Maybe (a, b)
nextPosFrom (x, y)
  | x == 8 = if y == 8 then Nothing else Just (0, y + 1)
  | otherwise = Just (x + 1, y)

findNextValidPos :: [[SudokuCell]] -> (Int, Int) -> Maybe (Int, Int)
findNextValidPos b p
  | hasValue b p = nextPosFrom p >>= findNextValidPos b
  | otherwise = Just p

-- when position is Nothing, we have solved the board
solve :: [[SudokuCell]] -> Maybe (Int, Int) -> Maybe [[SudokuCell]]
solve b Nothing = Just b
-- p is always the (valid) current point
solve b (Just p) = solveHelper b (getMissingFromPos b p) p

-- loop over a list of numbers until solved
solveHelper :: [[SudokuCell]] -> [SudokuCell] -> (Int, Int) -> Maybe [[SudokuCell]]
solveHelper b [] _ = Nothing
solveHelper b (x : xs) p = case solve mb np of
  Nothing -> solveHelper b xs p -- try next in current position
  s -> s
  where
    mb = modifyBoard b p x -- try to solve with the given number
    np = findNextValidPos mb p -- and go to the next position

solveSudoku :: [[SudokuCell]] -> Maybe [[SudokuCell]]
solveSudoku b = solve b (findNextValidPos b (0, 0))

solveAndPrint :: [[SudokuCell]] -> IO ()
solveAndPrint b = case solveSudoku b of
  Just s -> prettyPrint s
  Nothing -> putStrLn "Could not solve"
