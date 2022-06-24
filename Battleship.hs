import Data.Char (ord)
import Data.List (permutations)


type Coord = (Int, Int)
type Battleship = [Coord]
type Board = [[Bool]]
type Player = String


boardSize = 10
minBattleshipLength = 2
maxBattleshipLength = 5

-- helper functions for modularity

-- Initialize the 10x10 board
initialiseBoard :: Board
initialiseBoard = replicate boardSize (replicate boardSize False)

-- Replace num-th element in a list
replace :: Int -> [a] -> a -> [a]
replace num xs x = take (num-1) xs ++ [x] ++ drop num xs

-- Select the num-th element in a list
select :: Int -> [a] -> a
select num xs = take num xs !! (num-1)

-- Split input containing coords seperated by semi-colons into a list of coords.
splitInput :: String -> [String]
splitInput [] = [[]]
splitInput (x:xs) = if x == ';' then
                                      [] : splitInput xs
                                  else
                                      (x : head (splitInput xs)) : tail (splitInput xs)

-- Convert String into Coords
-- subtract ord 0 as its default ascii value is 48
convertCoord :: String -> Coord
convertCoord ['(', x, ',', y, ')'] = (ord x - ord '0' + 1, ord y - ord '0' + 1)
convertCoord _ = (-1, -1)

-- Check if a coord lies inside the board
checkCoord :: Coord -> Bool
checkCoord coord = and [  fst coord >= 1,
                          snd coord >= 1,
                          fst coord <= boardSize,
                          snd coord <= boardSize
                        ]

-- Mark a cell on the board as shot
markBlast :: Board -> Int -> Int -> Board
markBlast board x y = replace x board (replace y (select x board) True)

-- Convert the board into a printable string
convertBoard :: Board -> [Battleship] -> Coord -> String
convertBoard board battleships coord
        | fst coord <= boardSize && snd coord <= boardSize = if select (fst coord) (select (snd coord) board) then
          if or [coord == coord | battleship <- battleships, coord <- battleship] then 'o' : convertBoard board battleships (fst coord + 1, snd coord)
          else 'x' : convertBoard board battleships (fst coord + 1, snd coord)
          else ' ' : convertBoard board battleships (fst coord + 1, snd coord)
        | snd coord <= boardSize = "H\nH" ++ convertBoard board battleships (1, snd coord + 1)
        | otherwise = []

-- Output the board in the terminal
printBoard :: String -> Board -> [Battleship] -> IO ()
printBoard playerName board battleships = do
                                      putStrLn (playerName ++ "'s board:")
                                      putStrLn (replicate (boardSize+2) 'H' ++ "\nH" ++ convertBoard board battleships (1, 1) ++ replicate (boardSize+1) 'H')
                                      putStrLn ""

-- Remove destroyed battleships
removeDestroyedBattleships :: [Battleship] -> [Battleship]
removeDestroyedBattleships [] = []
removeDestroyedBattleships (x:xs) | null x    = removeDestroyedBattleships xs
                            | otherwise = x : removeDestroyedBattleships xs


-- Input:
--    coord:      The coord that is being shot at
--    battleship:       The battleship that we should check the coord against
--    board:      The board on which the battleship is located
--
-- Output:
--    Boolean Tuple that indicates if the shot was a miss or hit.
--    Destroyed Battleship returned as empty list
--
checkDestroyedBattleship :: Board -> Battleship -> Coord -> (Battleship, Bool)
checkDestroyedBattleship board battleship coord = if not (or [coord == coord | coord <- battleship]) then do
                                        (battleship, False)    -- Miss
                                      else do
                                        if not (and [select (fst coord) (select (snd coord) board) | coord <- battleship, coord /= coord]) then
                                            (battleship, True) -- Hit, but not sunk
                                        else
                                            ([], True)   -- Hit and sunk

-- Fire a shot at a given coord
--
-- Input:
--    coord: The position that we are shooting at
--    enemyBattleships: A list of all the opponent battleships
--    enemyBoard: The 10x10 board of the opponent
--
-- Output:
--    Tuple with the updated enemyBoard, enemyBattleships and a boolean to indicate a hit or miss
--
blast :: (Board, [Battleship]) -> Coord -> (Board, [Battleship], Bool)
blast (enemyBoard, enemyBattleships) coord = (markBlast enemyBoard (snd coord) (fst coord),
                                            removeDestroyedBattleships [fst (checkDestroyedBattleship enemyBoard battleship coord) | battleship <- enemyBattleships],
                                            or [snd (checkDestroyedBattleship enemyBoard battleship coord) | battleship <- enemyBattleships])


-- Fire at the opponent once for every battleship you have left
--
-- Input:
--    enemyBoard: Current board of the opponent
--    enemyBattleships: The list of all battleships from the opponent
--    ourBattleships:   List of battleship that we have left that can still blast
--
-- Output:
--    Tuple containing the updated board and battleships of the opponent
--
blastWithEveryBattleship :: (Board, [Battleship]) -> [Battleship] -> IO (Board, [Battleship])
blastWithEveryBattleship (enemyBoard, enemyBattleships) [] = return (enemyBoard, enemyBattleships)
blastWithEveryBattleship (enemyBoard, enemyBattleships) ourBattleships = do
                                                        putStrLn ("Enter the coords to blast shot (" ++ show (length ourBattleships) ++ " shots left)")
                                                        string <- getLine
                                                        let coord = convertCoord string
                                                        if checkCoord coord then
                                                            do
                                                              let (newEnemyBoard, newEnemyBattleships, hit) = blast (enemyBoard, enemyBattleships) coord

                                                              if hit then
                                                                  putStrLn ("Firing at coord (" ++ show (fst coord - 1) ++ "," ++ show (snd coord - 1) ++ "), Hit")
                                                              else
                                                                  putStrLn ("Firing at coord (" ++ show (fst coord - 1) ++ "," ++ show (snd coord - 1) ++ "), Miss")

                                                              if length newEnemyBattleships < length enemyBattleships then
                                                                  do
                                                                    putStrLn "You sunk my battleship!"
                                                                    blastWithEveryBattleship (newEnemyBoard, newEnemyBattleships) (tail ourBattleships)
                                                              else
                                                                  blastWithEveryBattleship (newEnemyBoard, newEnemyBattleships) (tail ourBattleships)
                                                        else
                                                            blastWithEveryBattleship (enemyBoard, enemyBattleships) ourBattleships

-- Input the names of the players
inputNames :: IO [String]
inputNames = do
               putStrLn "Enter Player 1 name"
               name1 <- getLine
               putStrLn "Enter Player 2 name"
               name2 <- getLine
               return [name1, name2]

-- Input one battleship with a given length
inputBattleship :: [Battleship] -> Int -> IO Battleship
inputBattleship placedBattleships len = do
                              putStrLn ("Enter battleship coords of length " ++ show len)
                              string <- getLine
                              let stringCoords = splitInput string
                              let coords = map convertCoord stringCoords
                              return coords

-- Input all the battleships for a player
inputBattleships :: Int -> [Battleship] -> IO [Battleship]
inputBattleships battleshipSize placedBattleships = if battleshipSize <= maxBattleshipLength then
                                      do
                                        battleship <- inputBattleship placedBattleships battleshipSize
                                        battleshipList <- inputBattleships (battleshipSize + 1) (battleship : placedBattleships)
                                        return (battleship : battleshipList)
                                  else
                                      return []


-- Play the game, one turn at a time
--
-- Input:
--    names:  List of player names
--    boards: List of boards belonging to the players
--    battleships:  List of battleships belonging to the player
--
-- The first element in the lists, are from the player whose turn it currently is
--
startGame :: [String] -> [Board] -> [[Battleship]] -> IO ()
startGame names boards battleships = do
                            putStrLn ("\num" ++ head names ++ "'s turn")
                            printBoard (last names) (last boards) (last battleships)
                            (newBoard, newBattleshipList) <- blastWithEveryBattleship (last boards, last battleships) (head battleships)
                            if null newBattleshipList then
                                do
                                  putStrLn ("\num" ++ head names ++ " won!\num")
                                  printBoard (last names) newBoard newBattleshipList
                                  printBoard (head names) (head boards) (head battleships)
                            else
                                startGame [last names, head names] [newBoard, head boards] [newBattleshipList, head battleships]

-- The entry point of the program
main :: IO ()
main = do
         names <- inputNames

         putStrLn (head names ++ ", enter battleships coords")
         battleshipsPlayer1 <- inputBattleships minBattleshipLength []

         putStrLn (last names ++ ", enter battleships coords")
         battleshipsPlayer2 <- inputBattleships minBattleshipLength []

         startGame names [initialiseBoard, initialiseBoard] [battleshipsPlayer1, battleshipsPlayer2]
