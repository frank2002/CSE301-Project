module Trees where

import Data.List
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

gTreeToTreePos :: GTree -> GTree -> DT.Tree String
gTreeToTreePos current (Node n label strings children) =
  let newLabel = if Node n label strings children == current
                 then label ++  " " ++ show strings ++ " <--you are here!"
                 else label ++  " " ++ show strings
  in DT.Node newLabel (map (gTreeToTreePos current) children)
gTreeToTreePos current Leaf =
  DT.Node (if Leaf == current then "Leaf <--you" else "Leaf") []

-- Print GTree
printGTree :: GTree -> IO ()
printGTree = putStrLn . DT.drawTree . gTreeToTree

printGTreePos :: GTree -> GTree -> IO ()
printGTreePos tree current = putStrLn . DT.drawTree $ gTreeToTreePos current tree

main :: IO ()
main = do
  let tree = Node 1 "root" ["a", "b"]
              [ Node 2 "child1" ["c"] 
                [ Node 6 "grandchild1" ["i"] []
                , Node 7 "grandchild2" ["j"] []
                , Node 8 "grandchild3" ["k"] []
                ]
              , Node 3 "child2" ["d", "e"]
                [ Node 4 "grandchild1" ["f"] []
                , Node 5 "grandchild2" ["g", "h"] []
                ]
              ]
      current = Node 4 "grandchild1" ["f"] []
--   printGTree tree

  printGTreePos tree current