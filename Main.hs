module Main where

import Trees
import Definition
import Armor
import Weapons
import Shoes
import Hero

import Data.List
import System.IO ( hFlush, stdout )
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)
import Data.Maybe (listToMaybe, mapMaybe, isJust)
import Control.Monad (forM_)
import Text.Read (readMaybe)
import System.Posix.Internals (puts)


-- Main loop
repl :: IO ()
repl = do
    putStrLn "Welcome to the game!\n"
    putStrLn "You can use the following commands:" -- not complete
    putStrLn "Let's start the game!\n\n"
    let initialState = GameState sampleTree [1] initialHero sampleTree False
    go initialState
    where
        go :: GameState -> IO ()
        go state@(GameState pos path hero tree win) = do
            case pos of
                Leaf -> putStrLn "You see a leaf."
                Node index name attrs children -> do
                    putStrLn $ "You are in a room named " ++ name ++ "."
                    putStrLn "This is your current map:\n   "
                    printVisitedTree tree index path
                    putStrLn ""
                    putStrLn "What's next?"
            putStr "> "
            hFlush stdout
            line <- getLine
            case readMaybe line of
                Just 1 -> handleCmd Go_Down state >>= go
                Just 2 -> handleCmd Go_Up state >>= go
                Just 3 -> handleCmd Battle state >>= go
                Just 4 -> handleCmd Search state >>= go
                Just 5 -> handleCmd Check state >>= go
                Just 6 -> handleCmd Quit state >>= go
            -- case parseInput parseCmd line of
            --     Nothing -> do
            --         putStrLn "I'm sorry, I do not understand."
            --         go state
            --     Just cmd -> handleCmd cmd state >>= go

handleCmd :: Cmd -> GameState -> IO GameState

handleCmd Go_Down state@(GameState pos path hero tree win) = do
    case pos of
        Node index _ attrs children -> do
            if defeated attrs
                then do
                    if null children
                        then do
                            putStrLn "You are at a leaf node. You cannot go down any further!\n"
                            return state
                        else do
                            putStrLn "This node is defeated. You can proceed."
                            displayChildren children
                            choice <- getUserChoice (length children)
                            case children !! (choice - 1) of 
                                Node index_child _ _ _ -> do
                                    let newPath = path ++ [index_child]
                                        newState = state { currentPos = children !! (choice - 1), path = newPath }
                                    putStrLn "You have entered the room.\n"
                                    return newState
                else do
                    putStrLn "This node is not defeated yet. You only battle or go back to the parent!\n"
                    return state
        Leaf -> do
            putStrLn "You are at a leaf node. You cannot go down any further.\n"
            return state
    where
        displayChildren :: [GTree] -> IO ()
        displayChildren children = do
            putStrLn "There are following rooms you can go down:"
            forM_ (zip [1..] children) $ \(i, Node _ label attrs _) -> do
                let status = if isJust (enemy attrs) && not (defeated attrs) then " (Enemy Here)" else ""
                putStrLn $ show i ++ ". " ++ label ++ status
            putStrLn "Choose which room you want to enter:"
            putStr "> "
            hFlush stdout
            

        getUserChoice :: Int -> IO Int
        getUserChoice n = do
            input <- getLine
            case readMaybe input of
                Just num | num > 0 && num <= n -> return num
                _ -> do
                    putStrLn "Invalid choice. Please enter a number between 1 and n."
                    putStr "> "
                    hFlush stdout
                    getUserChoice n


handleCmd Go_Up state@(GameState pos path hero tree win) = do
    case findParent tree pos of
        Nothing -> do
            putStrLn "You are already at the root. You cannot go up any further.\n"
            return state
        Just parent -> do
            case parent of
                Node index _ _ _ -> do
                    let newPath = path ++ [index]
                        newState = state { currentPos = parent, path = newPath }
                    putStrLn "You climb up to the parent node.\n"
                    return newState
    where
        findParent :: GTree -> GTree -> Maybe GTree
        findParent (Node n label attrs children) target =
            if target `elem` children then Just (Node n label attrs children)
            else listToMaybe $ mapMaybe (`findParent` target) children
        findParent Leaf _ = Nothing


-- handleCmd Battle state@(GameState pos path hero tree win) = -- Implement logic for battle
-- handleCmd Search state@(GameState pos path hero tree win) = -- Implement logic for search
    -- Display weapon and armor
-- handleCmd Check state@(GameState pos path attrs tree) = -- Implement logic for check
handleCmd Quit state@(GameState pos path hero tree win) = do
  putStrLn "Goodbye."
  exitSuccess



main :: IO ()
main = repl
