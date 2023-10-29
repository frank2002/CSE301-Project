module Shoes
(
  Shoe(..)
  ,shoe_list
) where

data Shoe = Shoe {shoeAttackBonus :: Int, shoeHealthBonus :: Int, shoeDefense :: Int, shoeSpeed::Int, shoeName :: String } deriving (Show,Eq)

AttackShoe = Shoe 30 0 0 45 "Attack Shoe"
DefenseShoe = Shoe 0 20 0 45 "Defense Shoe"
HealthShoe = Shoe 0 0 50 45 "Health Shoe"

shoe_list = [AttackShoe, DefenseShoe, HealthShoe]
