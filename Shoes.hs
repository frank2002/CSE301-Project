module Shoes
(
  Shoe(..)
  ,first_shoe
  ,shoe_list
) where

data Shoe = Shoe {shoeAttackBonus :: Int, shoeHealthBonus :: Int, shoeDefense :: Int, shoeSpeed::Int, shoeName :: String } deriving (Show,Eq)
first_shoe = Shoe 0 0 0 20 "First Shoe"

attackShoe = Shoe 30 0 0 45 "Attack Shoe"
defenseShoe = Shoe 0 20 0 45 "Defense Shoe"
healthShoe = Shoe 0 0 50 45 "Health Shoe"

shoe_list = [attackShoe, defenseShoe, healthShoe]
