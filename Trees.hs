module Trees where

import           Data.List
import           Data.Maybe (catMaybes)
import qualified Data.Tree  as DT
import           Diagrams   (D)

import           Armor
import           Enemies
import           Shoes
import           Weapons

data NodeAttributes = NodeAttributes
  { enemy    :: Maybe Enemy
  , armor    :: Maybe Armor
  , weapon   :: Maybe Weapon
  , shoes    :: Maybe Shoe
  , defeated :: Bool
  , exit     :: Bool
  } deriving (Show, Eq)

data GTree
  = Node Int String NodeAttributes [GTree]
  | Leaf
  deriving (Eq)

-- Node: Represents a tree node. It takes four arguments:
-- An Int to store an integer value.
-- A String to store a string value.
-- A NodeAttributes to store the attributes of the node.
-- A [GTree] to store the children of the node, which are themselves trees.
-- Convert GTree to Tree String
gTreeToTree :: GTree -> DT.Tree String
gTreeToTree (Node _ label attrs children) =
  let status =
        case enemy attrs of
          Just _ ->
            if defeated attrs
              then " (Enemy Defeated)"
              else " (Enemy Here)"
          Nothing -> ""
   in DT.Node (label ++ status) (map gTreeToTree children)
gTreeToTree Leaf = DT.Node "Leaf" []

gTreeToTreePos :: Int -> GTree -> DT.Tree String
gTreeToTreePos current (Node n label attrs children) =
  let status =
        case enemy attrs of
          Just _ ->
            if defeated attrs
              then " (Enemy Defeated)"
              else " (Enemy Here!)"
          Nothing -> ""
      currentPosition =
        if n == current
          then " <--- you are here!"
          else ""
      childrenTrees = map (gTreeToTreePos current) children
   in DT.Node (label ++ status ++ currentPosition) childrenTrees
gTreeToTreePos _ Leaf = DT.Node "Leaf" []

printGTree :: GTree -> IO ()
printGTree = putStrLn . DT.drawTree . gTreeToTree

printGTreePos :: GTree -> Int -> IO ()
printGTreePos tree current =
  putStrLn . DT.drawTree $ gTreeToTreePos current tree

filterTree :: GTree -> [Int] -> Maybe GTree
filterTree (Node n label attrs children) indices
  | n `elem` indices =
    Just $ Node n label attrs (catMaybes $ map (markUnseen indices) children)
  | otherwise = Nothing
filterTree Leaf _ = Just Leaf

markUnseen :: [Int] -> GTree -> Maybe GTree
markUnseen visited (Node n label attrs children)
  | n `notElem` visited = Just $ Node n (label ++ " (Unexplored)") attrs []
  | otherwise =
    Just $ Node n label attrs (catMaybes $ map (markUnseen visited) children)
markUnseen _ Leaf = Just Leaf

printVisitedTree :: GTree -> Int -> [Int] -> IO ()
printVisitedTree tree current indices =
  case filterTree tree indices of
    Just filteredTree -> printGTreePos filteredTree current
    Nothing -> putStrLn "The character has not visited any nodes in the tree."
