module Main where

import qualified MyLib (someFunc)
import Data.Char (ord)
import Data.List (permutations)

type Coord = (Int,Int)
type Ship = [Coord]
type Player = String

boardSize = 10

-- Player Name Inputs
nameInput :: IO [String]
nameInput = do
  putStrLn "Input name of Player 1"
  player1 <- getLine
  putStrLn "Input name of Player 2"
  player2 <- getLine
  putStrLn "Welcome " ++ player1 ++ " and " ++ player2
  return [player1,player2]

-- Single Ship Input
shipInput :: [Ship] -> Int -> IO Ship
shipInput shipCoord len = do
    putStrLn "Enter the coordinates of ship length " ++ show len 
    line <- getLine
    
-- Convert coordinates from string into a list of strings
convertStringToList :: String -> [String]
convertStringToList [] = [[]]
convertStringToList (x:xs) =  if x == ';' then
                               [] : convertStringToList xs
                              else
                                (x : head(convertStringToList xs)) : tail (convertStringToList xs)

convertStringToCoord :: String -> Coord
convertStringToCoord ['(',x,',',y,')'] = (ord x - ord '0', ord y - ord '0') -- ascii value of 0 is 48
convertStringToCoord _ = (-1,-1) -- error handling


-- Main Function
main :: IO ()
main = do
  names <- nameInput

  putStrLn (head names) ++ ", enter ship coordinates"


  putStrLn "Hello, Haskell!"
  MyLib.someFunc
