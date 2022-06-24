import Data.Char (ord)
import Data.List (permutations)


type Coord = (Int, Int)
type Battleship = [Coord]
type Board = [[Bool]]
type Player = String


board = 10
minShipLength = 2
maxShipLength = 5

-- helper functions for modularity

-- Select the num-th element in a list
select :: Int -> [a] -> a
select num xs = take num xs !! (num-1)

-- Replace num-th element in a list
replace :: Int -> [a] -> a -> [a]
replace num xs x = take (num-1) xs ++ [x] ++ drop num xs

-- Initialize the 10x10 board
initialiseBoard :: Board
initialiseBoard = replicate board (replicate board False)

-- Extract the coordinate from the string
-- Also immediately convert the coordinate from range [0,10[ to [1,10]
-- An invalid coordinate is returned when the string isn't of the correct style.
convertStringToCoordinates :: String -> Coord
convertStringToCoordinates ['(', x, ',', y, ')'] = (ord x - ord '0' + 1, ord y - ord '0' + 1)
convertStringToCoordinates _ = (-1, -1)

-- Split a string containing coordinates seperated by semi-colons into a list of (unchecked) coordinates.
-- You must still call convertStringToCoordinates on every element in the returned list.
splitCoordinatesInString :: String -> [String]
splitCoordinatesInString [] = [[]]
splitCoordinatesInString (x:xs) = if x == ';' then
                                      [] : splitCoordinatesInString xs
                                  else
                                      (x : head (splitCoordinatesInString xs)) : tail (splitCoordinatesInString xs)

-- Check if a coordinate lies inside the board
validateCoordinate :: Coord -> Bool
validateCoordinate coord = and [ fst coord >= 1,
                                 snd coord >= 1,
                                 fst coord <= board,
                                 snd coord <= board
                               ]

-- Convert the board into a printable string
convertFieldToString :: Board -> [Battleship] -> Coord -> String
convertFieldToString board ships coordinate
        | fst coordinate <= board
          && snd coordinate <= board = if select (fst coordinate) (select (snd coordinate) board) then
                                               if or [coordinate == coord | ship <- ships, coord <- ship] then 'o' : convertFieldToString board ships (fst coordinate + 1, snd coordinate)
                                                   else 'x' : convertFieldToString board ships (fst coordinate + 1, snd coordinate)
                                           else ' ' : convertFieldToString board ships (fst coordinate + 1, snd coordinate)
                                        
        | snd coordinate <= board = "H\nH" ++ convertFieldToString board ships (1, snd coordinate + 1)
        | otherwise = []

-- Output the board in the terminal
printField :: String -> Board -> [Battleship] -> IO ()
printField playerName board ships = do
                                      putStrLn (playerName ++ "'s board:")
                                      putStrLn (replicate (board+2) 'H' ++ "\nH" ++ convertFieldToString board ships (1, 1) ++ replicate (board+1) 'H')
                                      putStrLn ""

-- Mark a cell on the board as shot
markShot :: Board -> Int -> Int -> Board
markShot board x y = replace x board (replace y (select x board) True)

-- Remove the ships from the list when they are destroyed
removeDestroyedShips :: [Battleship] -> [Battleship]
removeDestroyedShips [] = []
removeDestroyedShips (x:xs) | null x    = removeDestroyedShips xs
                            | otherwise = x : removeDestroyedShips xs

-- Check if the ship has been destroyed and remove it from the game when it is
--
-- Input:
--    board:      The board on which the ship is located
--    ship:       The ship that we should check the coordinate against
--    coordinate: The coordinate that is being shot at
--
-- Output:
--    Tuple of the ship that was given as input and a boolean that indicates if the shot was a hit or miss.
--    When the ship is sunk, an empty list will be returned instead of the ship that was given as input.
--
checkShipDestroyed :: Board -> Battleship -> Coord -> (Battleship, Bool)
checkShipDestroyed board ship coordinate = if not (or [coordinate == coord | coord <- ship]) then do
                                               (ship, False)    -- Miss
                                           else do
                                               if not (and [select (fst coord) (select (snd coord) board) | coord <- ship, coord /= coordinate]) then
                                                   (ship, True) -- Hit, but not sunk
                                               else
                                                   ([], True)   -- Hit and sunk


-- Fire a shot at a given coordinate
--
-- Input:
--    enemyField: The 10x10 board of the opponent
--    enemyShips: A list of all the opponent ships
--    coordinate: The position that we are shooting at
--
-- Output:
--    Tuple with the updated enemyField, enemyShips and a boolean to indicate a hit or miss
--
fire :: (Board, [Battleship]) -> Coord -> (Board, [Battleship], Bool)
fire (enemyField, enemyShips) coordinate = (markShot enemyField (snd coordinate) (fst coordinate),
                                            removeDestroyedShips [fst (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips],
                                            or [snd (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips])


-- Fire at the opponent once for every ship you have left
--
-- Input:
--    enemyField: Current board of the opponent
--    enemyShips: The list of all ships from the opponent
--    ourShips:   List of ship that we have left that can still fire
--
-- Output:
--    Tuple containing the updated board and ships of the opponent
--
fireWithEveryShip :: (Board, [Battleship]) -> [Battleship] -> IO (Board, [Battleship])
fireWithEveryShip (enemyField, enemyShips) [] = return (enemyField, enemyShips)
fireWithEveryShip (enemyField, enemyShips) ourShips = do
                                                        putStrLn ("Enter the coordinates to fire shot (" ++ show (length ourShips) ++ " shots left)")
                                                        string <- getLine
                                                        let coord = convertStringToCoordinates string
                                                        if validateCoordinate coord then
                                                            do
                                                              let (newEnemyField, newEnemyShips, hit) = fire (enemyField, enemyShips) coord

                                                              if hit then
                                                                  putStrLn ("Firing at coordinate (" ++ show (fst coord - 1) ++ "," ++ show (snd coord - 1) ++ "), Hit")
                                                              else
                                                                  putStrLn ("Firing at coordinate (" ++ show (fst coord - 1) ++ "," ++ show (snd coord - 1) ++ "), Miss")

                                                              if length newEnemyShips < length enemyShips then
                                                                  do
                                                                    putStrLn "You sunk my battleship!"
                                                                    fireWithEveryShip (newEnemyField, newEnemyShips) (tail ourShips)
                                                              else
                                                                  fireWithEveryShip (newEnemyField, newEnemyShips) (tail ourShips)
                                                        else
                                                            fireWithEveryShip (enemyField, enemyShips) ourShips

-- Play the game, one turn at a time
--
-- Input:
--    names:  List of player names
--    fields: List of fields belonging to the players
--    ships:  List of ships belonging to the player
--
-- The first element in the lists, are from the player whose turn it currently is
--
play :: [String] -> [Board] -> [[Battleship]] -> IO ()
play names fields ships = do
                            putStrLn ("\num" ++ head names ++ "'s turn")
                            printField (last names) (last fields) (last ships)
                            (newField, newShipList) <- fireWithEveryShip (last fields, last ships) (head ships)
                            if null newShipList then
                                do
                                  putStrLn ("\num" ++ head names ++ " won!\num")
                                  printField (last names) newField newShipList
                                  printField (head names) (head fields) (head ships)
                            else
                                play [last names, head names] [newField, head fields] [newShipList, head ships]

-- Input one ship with a given length
inputShip :: [Battleship] -> Int -> IO Battleship
inputShip placedShips len = do
                              putStrLn ("Enter the coordinates of the ship of length " ++ show len ++ "?")
                              string <- getLine
                              let stringCoords = splitCoordinatesInString string
                              let coords = map convertStringToCoordinates stringCoords
                              return coords

-- Input all the ships for a player
inputShips :: Int -> [Battleship] -> IO [Battleship]
inputShips shipSize placedShips = if shipSize <= maxShipLength then
                                      do
                                        ship <- inputShip placedShips shipSize
                                        shipList <- inputShips (shipSize + 1) (ship : placedShips)
                                        return (ship : shipList)
                                  else
                                      return []

-- Input the names of the players
inputNames :: IO [String]
inputNames = do
               putStrLn "What is the name of the first player?"
               name1 <- getLine
               putStrLn "What is the name of the second player?"
               name2 <- getLine
               return [name1, name2]

-- The entry point of the program
main :: IO ()
main = do
         names <- inputNames

         putStrLn (head names ++ ", enter your ships")
         shipsPlayer1 <- inputShips minShipLength []

         putStrLn (last names ++ ", enter your ships")
         shipsPlayer2 <- inputShips minShipLength []

         play names [initialiseBoard, initialiseBoard] [shipsPlayer1, shipsPlayer2]
