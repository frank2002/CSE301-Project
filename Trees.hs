module Trees where

import Data.List
import Data.Maybe (catMaybes)
import qualified Data.Tree as DT
import Diagrams (D)

import Armor
import Weapons
import Shoes
import Enemies

data NodeAttributes = NodeAttributes {
  enemy :: Maybe Enemy,
  armor :: Maybe Armor,
  weapon :: Maybe Weapon,
  shoes :: Maybe Shoe,
  defeated :: Bool,
  exit :: Bool
} deriving (Show, Eq)


data GTree = Node Int String NodeAttributes [GTree]
           | Leaf
           deriving (Eq)

-- Node: Represents a tree node. It takes four arguments:
-- An Int to store an integer value.
-- A String to store a string value.
-- A [String] to store a list of strings.
-- A [Tree] to store the children of the node, which are themselves trees.


-- Convert GTree to Tree String
gTreeToTree :: GTree -> DT.Tree String
gTreeToTree (Node _ label attrs children) =
    let status = case enemy attrs of
                 Just _ -> if defeated attrs then " (Enemy Defeated)" else " (Enemy Here)"
                 Nothing -> ""
  in DT.Node (label ++ status) (map gTreeToTree children)
gTreeToTree Leaf = DT.Node "Leaf" []


gTreeToTreePos :: Int -> GTree -> DT.Tree String
gTreeToTreePos current (Node n label attrs children) =
  let status = case enemy attrs of
                 Just _ -> if defeated attrs then " (Enemy Defeated)" else " (Enemy Here!)"
                 Nothing -> ""
      currentPosition = if n == current then " <--- you are here!" else ""
      childrenTrees = map (gTreeToTreePos current) children
  in DT.Node (label ++ status ++ currentPosition) childrenTrees
gTreeToTreePos _ Leaf = DT.Node "Leaf" []

-- Print GTree
printGTree :: GTree -> IO ()
printGTree = putStrLn . DT.drawTree . gTreeToTree

printGTreePos :: GTree -> Int -> IO ()
printGTreePos tree current = putStrLn . DT.drawTree $ gTreeToTreePos current tree


filterTree :: GTree -> [Int] -> Maybe GTree
filterTree (Node n label attrs children) indices
  | n `elem` indices = Just $ Node n label attrs (catMaybes $ map (markUnseen indices) children)
  | otherwise = Nothing
filterTree Leaf _ = Just Leaf

markUnseen :: [Int] -> GTree -> Maybe GTree
markUnseen visited (Node n label attrs children)
  | n `notElem` visited = Just $ Node n (label ++ " (Unexplored)") attrs []
  | otherwise = Just $ Node n label attrs (catMaybes $ map (markUnseen visited) children)
markUnseen _ Leaf = Just Leaf

printVisitedTree :: GTree -> Int -> [Int] -> IO ()
printVisitedTree tree current indices = case filterTree tree indices of
  Just filteredTree -> printGTreePos filteredTree current
  Nothing -> putStrLn "The character has not visited any nodes in the tree."


-- Example Tree
main :: IO ()
main = do
  let sampleTree = Node 1 "Entrance" (NodeAttributes Nothing (Just (armor_list !! 0)) Nothing Nothing False False)
        [ Node 2 "Hallway" (NodeAttributes (Just goblin) (Just (armor_list !! 1)) (Just (weapon_list !! 0)) (Just (shoe_list !! 0)) True False)
          [ Node 6 "Hidden Room" (NodeAttributes Nothing (Just (armor_list !! 2)) (Just (weapon_list !! 1)) (Just (shoe_list !! 1)) False False) []
          , Node 7 "Guard Room" (NodeAttributes (Just parademon) (Just (armor_list !! 3)) (Just (weapon_list !! 2)) (Just (shoe_list !! 2)) False False)
            [ Node 11 "Secret Vault" (NodeAttributes Nothing (Just (armor_list !! 4)) (Just (weapon_list !! 3)) (Just (shoe_list !! 0)) False False) []
            , Node 12 "Armory" (NodeAttributes Nothing (Just (armor_list !! 5)) (Just (weapon_list !! 4)) (Just (shoe_list !! 2)) False False) []
            ]
          ]
        , Node 3 "Treasure Room" (NodeAttributes Nothing (Just (armor_list !! 6)) (Just (weapon_list !! 5)) (Just (shoe_list !! 0)) False False) []
        , Node 4 "Dungeon" (NodeAttributes (Just parademon) (Just (armor_list !! 7)) (Just (weapon_list !! 6)) (Just (shoe_list !! 1)) False False)
          [ Node 8 "Torture Room" (NodeAttributes (Just goblin) (Just (armor_list !! 8)) (Just (weapon_list !! 7)) (Just (shoe_list !! 2)) False False) []
          , Node 9 "Storage Room" (NodeAttributes Nothing (Just (armor_list !! 9)) (Just (weapon_list !! 8)) (Just (shoe_list !! 1)) False False) []
          ]
        , Node 5 "Exit" (NodeAttributes Nothing Nothing Nothing Nothing False True) []
        ]
      visitedNodes = [1, 2,3]
      current = 11
  printVisitedTree sampleTree current visitedNodes
  printGTreePos sampleTree current

-- main :: IO ()
-- main = do
--   let tree = Node 1 "root" ["a", "b"]
--               [ Node 2 "child1" ["c"] 
--                 [ Node 6 "grandchild1" ["i"] []
--                 , Node 7 "grandchild2" ["j"] []
--                 , Node 8 "grandchild3" ["k"] []
--                 ]
--               , Node 3 "child2" ["d", "e"]
--                 [ Node 4 "grandchild1" ["f"] []
--                 , Node 5 "grandchild2" ["g", "h"] []
--                 ]
--               ]
--       current = 1

-- --   printGTree tree

--   printGTreePos tree current