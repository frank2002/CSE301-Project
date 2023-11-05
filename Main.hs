module Main where

import           Armor
import           Definition
import           Enemies
import           Game
import           Hero
import           Parser
import           Shoes
import           Trees
import           Weapons

import           Control.Concurrent      (threadDelay)
import           Control.Monad           (forM_, when)
import           Control.Monad.RWS.Class (MonadState (put))
import           Data.List
import           Data.Maybe              (isJust, isNothing, listToMaybe,
                                          mapMaybe)
import           System.Exit             (exitSuccess)
import           System.IO               (hFlush, stdout)
import           System.Posix.Internals  (puts)
import qualified System.Random           as SR
import           Text.Read               (readMaybe)

-- Main loop
repl :: IO ()
repl = do
  putStrLn "Welcome to the game Infinite Blade!\n"
  putStrLn "\t****************************************************"
  putStrLn "\t******************** Background ********************"
  putStrLn "\t****************************************************"
  putStrLn "\nYou are at the entrance (root) of an ancient tree-like maze."
  putStrLn "There are some enemies in the maze. Try to defeat them!"
  putStrLn
    "You need to find the exit of this ruined castle, defeat the deathless, and get the Infinity Blade to win the game.\n"
  putStrLn
    "Do not forget to search for equipment to help you in your journey.\n"
  putStrLn "\t****************************************************"
  putStrLn "\t********************** Rules ***********************"
  putStrLn "\t****************************************************"
  putStrLn
    "\nYou can go down to the children nodes, go up to the parent node, battle the enemy, search for equipment, "
  putStrLn "check the current status of you and your enemy, or quit the game."
  putStrLn
    "You can only go down to a child node if the enemy in the current node is defeated."
  putStrLn
    "You can only search for equipment if the enemy in the current node is defeated."
  putStrLn "You will win if you find the exit.\n"
  putStrLn "\t****************************************************"
  putStrLn "\t****************** Instructions ********************"
  putStrLn "\t****************************************************"
  putStrLn "\nYou can use the following commnads:"
  putStrLn
    "1. <go down to the first child> / <go down to child one>: go down to the child node with the given number. "
  putStrLn "2. <go up>: go up to the parent node."
  putStrLn "3. <battle>: battle the enemy in the current node."
  putStrLn "4. <search>: search for equipment in the current node."
  putStrLn "5. <check>: check the current status of you and your enemy."
  putStrLn "6. <quit>: quit the game.\n"
  putStrLn "\t****************************************************"
  putStrLn "\n\nLet's start the game!\n\n"
  let initialState = GameState sampleTree [1] initialHero sampleTree False
  go initialState
  where
    go :: GameState -> IO ()
    go state@(GameState pos path hero tree win) = do
      case pos of
        Leaf -> putStrLn "You see a leaf."
        Node index name attrs children -> do
          putStrLn "****************************************************"
          putStrLn $ "You are in a room named " ++ name ++ "."
          putStrLn "This is your current map:\n   "
          printVisitedTree tree index path
          putStrLn ""
          putStrLn "What's next?"
      putStr "> "
      hFlush stdout
      line <- getLine
      putStrLn "\n****************************************************"
            -- case readMaybe line of
                -- Just 1 -> handleCmd Go_Down state >>= go
                -- Just 2 -> handleCmd Go_Up state >>= go
                -- Just 3 -> handleCmd Battle state >>= go
                -- Just 4 -> handleCmd Search state >>= go
                -- Just 5 -> handleCmd Check state >>= go
                -- Just 6 -> handleCmd Quit state >>= go
      case parseInput parseCmd line of
        Nothing -> do
          putStrLn "I'm sorry, I do not understand."
          go state
        Just cmd -> handleCmd cmd state >>= go

handleCmd :: Cmd -> GameState -> IO GameState
handleCmd (Go_Down choice) state@(GameState pos path hero tree win) = do
  case pos of
    Node index _ attrs children -> do
      if defeated attrs
        then do
          if null children
            then do
              putStrLn
                "You are at a leaf node. You cannot go down any further!\n"
              return state
            else if choice > 0 && choice <= length children
                   then do
                     let nextNode = children !! (choice - 1)
                     case nextNode of
                       Node index_child _ attrs_child _ -> do
                         if exit attrs_child
                           then do
                             putStrLn
                               "Congratulations! You have found the exit and won the game!\n"
                             exitSuccess
                           else do
                             let newPath = path ++ [index_child]
                                 newState =
                                   state {currentPos = nextNode, path = newPath}
                             putStrLn
                               $ "You have entered the room: "
                                   ++ show choice
                                   ++ ".\n"
                             return newState
                       Leaf -> do
                         putStrLn "You have reached a leaf node.\n"
                         return state
                   else do
                     putStrLn "Invalid room number. Please try again.\n"
                     return state
        else do
          putStrLn
            "This node is not defeated yet. You can only battle or go back to the parent!\n"
          return state
    Leaf -> do
      putStrLn "You are at a leaf node. You cannot go down any further.\n"
      return state
handleCmd Go_Up state@(GameState pos path hero tree win) = do
  case findParent tree pos of
    Nothing -> do
      putStrLn "You are already at the root. You cannot go up any further.\n"
      return state
    Just parent -> do
      case parent of
        Node index _ _ _ -> do
          let newPath = path ++ [index]
              newState = state {currentPos = parent, path = newPath}
          putStrLn "You climb up to the parent node.\n"
          return newState
  where
    findParent :: GTree -> GTree -> Maybe GTree
    findParent (Node n label attrs children) target =
      if target `elem` children
        then Just (Node n label attrs children)
        else listToMaybe $ mapMaybe (`findParent` target) children
    findParent Leaf _ = Nothing
handleCmd Battle state@(GameState pos path hero tree win) = do
  case pos of
    Node _ _ (NodeAttributes (Just enemy) _ _ _ False _) _ -> do
      putStrLn "You have encountered an enemy! Prepare for battle."
      gen <- SR.newStdGen
      result <- fightEnemy hero enemy gen 1
      case result of
        Nothing -> do
          putStrLn "You have been defeated. Game Over."
          exitSuccess
        Just newHero -> do
          putStrLn "You have defeated the enemy!"
          putStrLn "You can search and continue your journey.\n"
          let newState = updateNodeAsDefeated pos state
          return newState {hero}
    Node _ _ (NodeAttributes (Just _) _ _ _ True _) _ -> do
      putStrLn "This enemy has already been defeated."
      return state
    _ -> do
      putStrLn "There is no enemy here to fight."
      return state
  where
    updateNodeAsDefeated :: GTree -> GameState -> GameState
    updateNodeAsDefeated targetNode state@(GameState pos path hero tree win) =
      let newTree = markNodeAsDefeated targetNode tree
          newPos =
            if pos == targetNode
              then updateNodeDefeatedFlag pos
              else pos
       in state {tree = newTree, currentPos = newPos}
    markNodeAsDefeated :: GTree -> GTree -> GTree
    markNodeAsDefeated targetNode (Node n label attrs children) =
      if targetNode == Node n label attrs children
        then Node n label (attrs {defeated = True}) children
        else Node n label attrs (map (markNodeAsDefeated targetNode) children)
    markNodeAsDefeated _ leaf = leaf
    updateNodeDefeatedFlag :: GTree -> GTree
    updateNodeDefeatedFlag (Node n label attrs children) =
      Node n label (attrs {defeated = True}) children
    updateNodeDefeatedFlag leaf = leaf
handleCmd Search state@(GameState pos path hero tree win) = do
  case pos of
    Node _ _ attrs _ -> do
      if not (defeated attrs)
        then do
          putStrLn "You cannot search the room until you defeat the enemy."
          return state
        else do
          putStrLn "Searching the room for equipment..."
          let noEquipment =
                isNothing (weapon attrs)
                  && isNothing (armor attrs)
                  && isNothing (shoes attrs)
          if noEquipment
            then do
              putStrLn "Sorry, there are no equipments in this room."
              return state
            else do
              putStrLn "You found some equipment!"
              displayInfoEquipment hero
              heroAfterWeapon <- checkAndEquipWeapon attrs hero
              heroAfterArmor <- checkAndEquipArmor attrs heroAfterWeapon
              heroAfterShoes <- checkAndEquipShoes attrs heroAfterArmor
              putStrLn "Updated Hero Information:"
              displayInfoEquipment heroAfterShoes
              return state {hero = heroAfterShoes}
    Leaf -> do
      putStrLn "There is nothing to search for at a leaf."
      return state
  where
    checkAndEquipWeapon :: NodeAttributes -> Hero -> IO Hero
    checkAndEquipWeapon attrs hero =
      case weapon attrs of
        Just newWeapon -> do
          putStrLn "You found a new weapon!"
          displayInfoWeapon newWeapon
          equipDecision <- askEquip "weapon"
          return
            $ if equipDecision
                then hero {currentWeapon = newWeapon}
                else hero
        Nothing -> return hero
    checkAndEquipArmor :: NodeAttributes -> Hero -> IO Hero
    checkAndEquipArmor attrs hero =
      case armor attrs of
        Just newArmor -> do
          putStrLn "You found new armor!"
          displayInfoArmor newArmor
          equipDecision <- askEquip "armor"
          return
            $ if equipDecision
                then hero {currentArmor = newArmor}
                else hero
        Nothing -> return hero
    checkAndEquipShoes :: NodeAttributes -> Hero -> IO Hero
    checkAndEquipShoes attrs hero =
      case shoes attrs of
        Just newShoes -> do
          putStrLn "You found new shoes!"
          displayInfoShoe newShoes
          equipDecision <- askEquip "shoes"
          return
            $ if equipDecision
                then hero {currentShoes = newShoes}
                else hero
        Nothing -> return hero
    askEquip :: String -> IO Bool
    askEquip itemType = do
      putStrLn $ "Do you want to equip the new " ++ itemType ++ "? (yes/no)"
      decision <- getLine
      return (decision == "yes" || decision == "Yes")
handleCmd Check state@(GameState pos path hero tree win) = do
  putStrLn "Checking current status..."
  displayInfoHero hero
  putStrLn ""
  case pos of
    Node _ _ attrs _ ->
      case enemy attrs of
        Just enemy -> do
          if defeated attrs
            then putStrLn "This enemy has already been defeated."
            else displayInfoEnemy enemy
        Nothing -> putStrLn "There is no enemy here."
    Leaf -> putStrLn "You are at a leaf node."
  return state
handleCmd Quit state@(GameState pos path hero tree win) = do
  putStrLn "Are you sure you want to quit? (y/n)"
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    "y" -> do
      putStrLn "You have not finished the game. See you next time!"
      exitSuccess
    "n" -> do
      putStrLn "You have chosen not to quit."
      return state
    _ -> do
      putStrLn "Invalid input. You have chosen not to quit."
      return state

main :: IO ()
main = repl
