{-
       File: animals.hs
     Author: Steven Tomcavage (stomcava@seas.upenn.edu)
       Date: November, 2010
Description: Animals question game with Haskell
-}

import Data.Char
import System.IO

------------------------------------------------------------------------------
-- Definition and methods for data structure used here
------------------------------------------------------------------------------

-- Defines a type for a binary tree that holds the questions and answers
data Tree a = 
        Answer String | 
        Question String (Tree a) (Tree a)
        deriving (Read, Show)

-- Defines where the file is located
filePath = "data.txt"
        
-- Reads the file containing the tree and de-serializes the tree
fileToTree :: (Read a) => FilePath -> IO a
fileToTree fp = do 
--	tree <- readFile fp 
    inFile <- openFile fp ReadMode
    tree <- hGetContents inFile
    tree `seq` hClose inFile
    return (read tree)

-- Writes the tree to a file after serializing it
treeToFile :: (Show a) => a -> FilePath -> IO ()
treeToFile tree fp = do writeFile fp (show tree)

-- Adds a new question and answer in the tree at the given path
treeInsert :: String -> String -> String -> Tree a -> Tree a
treeInsert newQ newA path (Question q yes no)
        | head path == 'y' = Question q (treeInsert newQ newA (tail path) yes) no
        | head path == 'n' = Question q yes (treeInsert newQ newA (tail path) no)
treeInsert newQ newA path (Answer a)
        | head path == 'y' = Question newQ (Answer newA) (Answer a)
        | head path == 'n' = Question newQ (Answer a) (Answer newA)
                
------------------------------------------------------------------------------
-- Main program starts here
------------------------------------------------------------------------------
        
-- Starts the game running
main = do
        animals <- fileToTree filePath
        putStrLn "Think of an animal. Hit Enter when you are ready.  "
        _ <- getLine
        newAnimals <- ask animals ""
        treeToFile newAnimals filePath
        putStrLn "Would you like to play again?"
        answer <- getLine
        if answer == "yes"
            then main
            else return ()

-- Walks through the animals tree and ask the question at each node
ask :: Tree a -> String -> IO (Tree a)
ask (Question q yes no) path = do
        putStrLn q
        answer <- getLine
        if answer == "yes" then ask yes ('y' : path)
                           else ask no  ('n' : path)
ask (Answer a) path = do
        putStrLn $ "I know! Is your animal a " ++ a ++ "?"
        answer <- getLine
        if answer == "yes" 
                then computerWins
                else playerWins a path

-- Prints a smug message and returns the unaltered tree
computerWins :: IO (Tree a)
computerWins = do 
        putStrLn "See? Humans should work, computers should think!"
        animals <- fileToTree filePath
        return (animals)

-- Adds the players animal and a question to distinquish the animal to the tree
playerWins :: String -> String -> IO (Tree a)
playerWins animal path = do 
        animals <- fileToTree filePath
        putStrLn "OK, what is your animal?"
        newAnimal <- getLine
        putStrLn $ "What question would distinquish your animal from a " ++ animal ++ "?"
        newQuestion <- getLine
        putStrLn $ "What is the answer for a " ++ newAnimal ++ "?"
        newAnswer <- getLine
        putStrLn "Thanks, I'll remember that!"
        if newAnswer == "yes"
                then return (treeInsert newQuestion newAnimal (reverse ('y' : path)) animals)
                else return (treeInsert newQuestion newAnimal (reverse ('n' : path)) animals)

