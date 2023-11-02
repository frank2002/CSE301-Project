module Definition where

import Data.List
import Trees

import Armor
import Weapons
import Shoes
import Hero
import Enemies




-- The game tree defined here
sampleTree :: GTree
sampleTree = Node 1 "Entrance" (NodeAttributes Nothing (Just (armor_list !! 0)) Nothing Nothing True False)
        [ Node 2 "Hallway" (NodeAttributes (Just goblin) (Just (armor_list !! 1)) (Just (weapon_list !! 0)) (Just (shoe_list !! 0)) True False)
          [ Node 6 "Hidden Room" (NodeAttributes Nothing (Just (armor_list !! 2)) (Just (weapon_list !! 1)) (Just (shoe_list !! 1)) True False) []
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
              -- This is an example



-- Command accepted by the game
data Cmd = Go_Down | Go_Up | Battle | Search | Check | Quit
  deriving (Show,Read)

-- Go_Down -> Go down to the one child of current node
-- Go_Up -> Go up to the parent of current node
-- Battle -> Battle with the monster
-- Search -> Search the node (armor, weapon, shoes), if any
-- Check -> Check the attributes of the enermy
-- Quit -> Quit the game


data GameState = GameState {
  currentPos :: GTree,
  path :: [Int],
  hero :: Hero,
  tree :: GTree,
  win :: Bool
}

initialHero :: Hero
initialHero = Hero {
    baseHealthPoints = 1,
    baseAttackPower = 0,
    baseSpeed = 50,
    baseDefense = 30,
    currentWeapon = first_weapon,    -- Fists initially
    currentArmor = first_armor,      -- No armor initially
    currentShoes = first_shoe        -- No shoes initially
}