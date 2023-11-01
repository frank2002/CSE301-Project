module Trees where

import Data.List
import Data.Maybe (catMaybes)
import qualified Data.Tree as DT
import Diagrams (D)

data GTree = Node Int String [String] [GTree]
           | Leaf
           deriving (Eq)

-- Node: Represents a tree node. It takes four arguments:
-- An Int to store an integer value.
-- A String to store a string value.
-- A [String] to store a list of strings.
-- A [Tree] to store the children of the node, which are themselves trees.


-- Convert GTree to Tree String
gTreeToTree :: GTree -> DT.Tree String
gTreeToTree (Node n label strings children) = DT.Node (label ++  " " ++ show strings) (map gTreeToTree children)
gTreeToTree Leaf = DT.Node "Leaf" []

-- gTreeToTreePos :: GTree -> GTree -> DT.Tree String
-- gTreeToTreePos current (Node n label strings children) =
--   let newLabel = if Node n label strings children == current
--                  then label ++  " " ++ show strings ++ " <--you are here!"
--                  else label ++  " " ++ show strings
--   in DT.Node newLabel (map (gTreeToTreePos current) children)
-- gTreeToTreePos current Leaf =
--   DT.Node (if Leaf == current then "Leaf <--you" else "Leaf") []

gTreeToTreePos :: Int -> GTree -> DT.Tree String
gTreeToTreePos current (Node n label strings children) =
  let newLabel = if n == current
                 then label ++ " " ++ show strings ++ " <--you are here!"
                 else label ++ " " ++ show strings
  in DT.Node newLabel (map (gTreeToTreePos current) children)
gTreeToTreePos _ Leaf = DT.Node "Leaf" []

-- Print GTree
printGTree :: GTree -> IO ()
printGTree = putStrLn . DT.drawTree . gTreeToTree

printGTreePos :: GTree -> Int -> IO ()
printGTreePos tree current = putStrLn . DT.drawTree $ gTreeToTreePos current tree


filterTree :: GTree -> [Int] -> Maybe GTree
filterTree (Node n label strings children) indices
  | n `elem` indices = Just $ Node n label strings (catMaybes $ map (`filterTree` indices) children)
  | otherwise = Nothing
filterTree Leaf _ = Just Leaf

printVisitedTree :: GTree -> Int -> [Int] -> IO ()
printVisitedTree tree current indices = case filterTree tree indices of
  Just filteredTree -> printGTreePos filteredTree current
  Nothing -> putStrLn "The character has not visited any nodes in the tree."


-- Example Tree
main :: IO ()
main = do
  let tree = Node 1 "root" ["a", "b"]
              [ Node 2 "child1" ["c"] 
                [ Node 6 "grandchild1" ["i"] []
                , Node 7 "grandchild2" ["j"] 
                  [ Node 11 "greatgrandchild1" ["m"] []
                  , Node 12 "greatgrandchild2" ["n"] []
                  ]
                , Node 8 "grandchild3" ["k"] []
                ]
              , Node 3 "child2" ["d", "e"]
                [ Node 4 "grandchild1" ["f"] 
                  [ Node 9 "greatgrandchild1" ["l"] []
                  ]
                , Node 5 "grandchild2" ["g", "h"] 
                  [ Node 10 "greatgrandchild1" ["o"] []
                  ]
                ]
              , Node 13 "child3" ["p", "q"]
                [ Node 14 "grandchild1" ["r"] []
                , Node 15 "grandchild2" ["s"] []
                ]
              ]
      visitedNodes = [1, 2,3,6,7,11,4,5,10]
      current = 11
  printVisitedTree tree current visitedNodes
  printGTreePos tree current

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