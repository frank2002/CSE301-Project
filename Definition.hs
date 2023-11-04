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
sampleTree = Node 1 "Entrance" (NodeAttributes Nothing Nothing Nothing Nothing True False)
        [ 
          Node 2 "First_trial" (NodeAttributes (Just goblin) Nothing (Just (weapon_list!! 0) ) Nothing False False)
          [
            Node 3 "Apokoliptian's Room" (NodeAttributes (Just apokoliptian) (Just (armor_list !! 1)) Nothing (Just (shoe_list !! 1)) False False) 
              [
                Node 6 "Treasure Room" (NodeAttributes Nothing Nothing Nothing (Just (shoe_list !! 0)) True False) [],
                Node 7 "Orc's Room" (NodeAttributes (Just orc) Nothing Nothing Nothing False False) 
                  [
                    Node 10 "Treasure Room" (NodeAttributes Nothing Nothing (Just (weapon_list !! 5)) Nothing True False) [],
                    Node 11 "MinotaurKing's Room" (NodeAttributes (Just minotaurKing) (Just (armor_list !! 4)) (Just (weapon_list !! 3)) Nothing False False) 
                      [
                        Node 12 "Treasure Room" (NodeAttributes Nothing Nothing Nothing (Just (shoe_list !! 2)) True False) [],
                        Node 13 "Darkload's Room" (NodeAttributes (Just darkload) (Just (armor_list !! 5)) Nothing Nothing False False) 
                          [
                            Node 14 "KingGuarder's Room" (NodeAttributes (Just kingGuarder) (Just (armor_list !! 7)) (Just (weapon_list !! 6)) Nothing False False) 
                              [
                                Node 16 "FireAmorload's Room" (NodeAttributes (Just fireAmorload) (Just (armor_list !! 9)) (Just (weapon_list !! 9)) Nothing False False) 
                                  [
                                    Node 19 "The Gold King" (NodeAttributes (Just theGodKing) Nothing Nothing Nothing False False) 
                                      [
                                        Node 21 "Exit" (NodeAttributes Nothing Nothing Nothing Nothing True True) []
                                      ],
                                    Node 20 "Deathless" (NodeAttributes (Just deathless) Nothing Nothing Nothing False False) 
                                      [
                                        Node 22 "Exit" (NodeAttributes Nothing Nothing Nothing Nothing True True) []
                                      ]
                                  ],
                                Node 17 "Treasure Room 1" (NodeAttributes Nothing Nothing (Just (weapon_list !! 8)) Nothing True False) [],
                                Node 18 "Treasure Room 2" (NodeAttributes Nothing (Just (armor_list !! 8)) Nothing Nothing True False) []
                              ],
                            Node 15 "Rice's Room" (NodeAttributes (Just rice) (Just (armor_list !! 6)) Nothing Nothing False False) []
                          ]

                      ]
                  ]
              ],
            Node 4 "Parademon's Room" (NodeAttributes (Just parademon) (Just (armor_list !! 0)) Nothing Nothing False False) [],
            Node 5 "Empty Room" (NodeAttributes Nothing Nothing Nothing Nothing True False) 
              [
                Node 8 "largeMinotaur's Room" (NodeAttributes (Just largeMinotaur) Nothing Nothing Nothing False False) [],
                Node 9 "Treasure Room" (NodeAttributes Nothing (Just (armor_list !! 2)) Nothing Nothing True False) []
              ]
              
          ]
        ]



-- Command accepted by the game
data Cmd = Go_Down Int | Go_Up | Battle | Search | Check | Quit
  deriving (Show, Read)

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
    baseHealthPoints = 10000,
    baseAttackPower = 10,
    baseSpeed = 50,
    baseDefense = 30,
    currentWeapon = first_weapon,    -- Fists initially
    currentArmor = first_armor,      -- No armor initially
    currentShoes = first_shoe        -- No shoes initially
}