module Hero
(
  Hero(..),
  Weapon(..),
  Armor(..),
  Shoe(..),
  deductHealth,
  changeWeapon,
  changeArmor,
  changeShoes,
  heroIsDead,
  totalAttackPower,
  totalHealthPoints,
  totalSpeed,
  totalDefense
) where

import Weapons
import Armor
import Shoes

data Hero = Hero {
    baseHealthPoints :: Int,
    baseAttackPower :: Int,
    baseSpeed :: Int,
    baseDefense :: Int,
    currentWeapon :: Weapon,
    currentArmor :: Armor,
    currentShoes :: Shoe
} deriving (Show, Eq)

-- Function to calculate total attack power of the hero
totalAttackPower :: Hero -> Int
totalAttackPower hero = baseAttackPower hero + weaponDamage (currentWeapon hero) + armorAttackBonus (currentArmor hero) + shoeAttackBonus (currentShoes hero)

-- Function to calculate total health points of the hero
totalHealthPoints :: Hero -> Int
totalHealthPoints hero = baseHealthPoints hero + armorHealthBonus (currentArmor hero) + shoeHealthBonus (currentShoes hero)

-- Function to calculate total speed of the hero
totalSpeed :: Hero -> Int
totalSpeed hero = baseSpeed hero + shoeSpeed (currentShoes hero)

-- Function to calculate total defense of the hero
totalDefense :: Hero -> Int
totalDefense hero = baseDefense hero + armorDefense (currentArmor hero) + shoeDefense (currentShoes hero)

-- Function to deduct health from the hero
deductHealth :: Hero -> Int -> Hero
deductHealth hero deduction = hero { baseHealthPoints = (totalHealthPoints hero) - deduction }

-- Function to change the hero's weapon
changeWeapon :: Hero -> Weapon -> Hero
changeWeapon hero newWeapon = hero { currentWeapon = newWeapon }

-- Function to change the hero's armor
changeArmor :: Hero -> Armor -> Hero
changeArmor hero newArmor = hero { currentArmor = newArmor }

-- Function to change the hero's shoes
changeShoes :: Hero -> Shoe -> Hero
changeShoes hero newShoes = hero { currentShoes = newShoes }

-- Function to check if the hero is dead
heroIsDead :: Hero -> Bool
heroIsDead hero = (totalHealthPoints hero) <= 0

-- Initial Hero
initialHero :: Hero
initialHero = Hero {
    baseHealthPoints = 50,
    baseAttackPower = 10,
    baseSpeed = 50,
    baseDefense = 30,
    currentWeapon = Weapon 0 0.0 "",  -- No weapon initially
    currentArmor = Armor 0 0 0 "",    -- No armor initially
    currentShoes = Shoe 0 0 0 0 ""   -- No shoes initially
}
