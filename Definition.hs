module Definition where

import Data.List
import Trees



-- The game tree defined here
gameTree :: GTree
gameTree = Node 1 "root" ["a", "b"]
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
              -- This is an example

-- Path of the character
path :: [Int]
path = [1] -- Start from root(Node 1)

-- Attributes of the character
-- defined here
attributes :: [String]
attributes = ["a", "b"] -- example


-- Command accepted by the game
data Cmd = Go_Down | Go_Up | Choose_child | Battle | Search | Check | Quit
  deriving (Show,Read)

-- Go_Down -> Go down to the child of current node
-- Go_Up -> Go up to the parent of current node
-- Choose_child -> Choose a child of current node to go to
-- Battle -> Battle with the monster
-- Search -> Search the node (armor, weapon, shoes), if any
-- Check -> Check the attributes of the enermy
-- Quit -> Quit the game