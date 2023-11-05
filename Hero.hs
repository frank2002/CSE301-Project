module Hero
  ( Hero(..)
  , Weapon(..)
  , Armor(..)
  , Shoe(..)
  , deductHealth
  , setHealth
  , changeWeapon
  , changeArmor
  , changeShoes
  , heroIsDead
  , totalAttackPower
  , totalHealthPoints
  , totalSpeed
  , totalDefense
  , displayInfoHero
  , displayInfoEquipment
  ) where

import           Armor
import           Shoes
import           Weapons

data Hero = Hero
  { baseHealthPoints :: Int
  , baseAttackPower  :: Int
  , baseSpeed        :: Int
  , baseDefense      :: Int
  , currentWeapon    :: Weapon
  , currentArmor     :: Armor
  , currentShoes     :: Shoe
  } deriving (Show, Eq)


-- Function to calculate total attack power of the hero
totalAttackPower :: Hero -> Int
totalAttackPower hero =
  baseAttackPower hero
    + weaponDamage (currentWeapon hero)
    + armorAttackBonus (currentArmor hero)
    + shoeAttackBonus (currentShoes hero)


-- Function to calculate total health points of the hero
totalHealthPoints :: Hero -> Int
totalHealthPoints hero =
  baseHealthPoints hero
    + armorHealthBonus (currentArmor hero)
    + shoeHealthBonus (currentShoes hero)


-- Function to calculate total speed of the hero
totalSpeed :: Hero -> Int
totalSpeed hero = baseSpeed hero + shoeSpeed (currentShoes hero)


-- Function to calculate total defense of the hero
totalDefense :: Hero -> Int
totalDefense hero =
  baseDefense hero
    + armorDefense (currentArmor hero)
    + shoeDefense (currentShoes hero)


-- Function to deduct health from the hero
deductHealth :: Hero -> Int -> Hero
deductHealth hero deduction =
  hero {baseHealthPoints = (baseHealthPoints hero) - deduction}


-- Function to add health to the hero, if the player wins the battle
setHealth :: Hero -> Int -> Hero
setHealth hero newHealth = hero {baseHealthPoints = newHealth}


-- Function to change the hero's weapon
changeWeapon :: Hero -> Weapon -> Hero
changeWeapon hero newWeapon = hero {currentWeapon = newWeapon}


-- Function to change the hero's armor
changeArmor :: Hero -> Armor -> Hero
changeArmor hero newArmor = hero {currentArmor = newArmor}


-- Function to change the hero's shoes
changeShoes :: Hero -> Shoe -> Hero
changeShoes hero newShoes = hero {currentShoes = newShoes}


-- Function to check if the hero is dead
heroIsDead :: Hero -> Bool
heroIsDead hero = (totalHealthPoints hero) <= 0


--------------------------------------------------------------------
-- Display information about the hero
--------------------------------------------------------------------
displayInfoHero :: Hero -> IO ()
displayInfoHero hero = do
  putStrLn "------------------------- Hero Stats -------------------------"
  putStrLn
    $ "Attack Power: "
        ++ show (totalAttackPower hero)
        ++ ", Health Points: "
        ++ show (totalHealthPoints hero)
        ++ ", Speed: "
        ++ show (totalSpeed hero)
        ++ ", Defense: "
        ++ show (totalDefense hero)
  displayInfoWeapon (currentWeapon hero)
  displayInfoArmor (currentArmor hero)
  displayInfoShoe (currentShoes hero)
  putStrLn "--------------------------------------------------------------"


--------------------------------------------------------------------
-- Display information about the equipment
--------------------------------------------------------------------
displayInfoEquipment :: Hero -> IO ()
displayInfoEquipment hero = do
  putStrLn "------------------------- Hero Equipment -------------------------"
  putStrLn
    $ "Weapon: "
        ++ weaponName (currentWeapon hero)
        ++ ", Armor: "
        ++ armorName (currentArmor hero)
        ++ ", Shoes: "
        ++ shoeName (currentShoes hero)
  -- Call the functions to display the information of the weapon, armor, and shoes
  displayInfoWeapon (currentWeapon hero)
  displayInfoArmor (currentArmor hero)
  displayInfoShoe (currentShoes hero)
  putStrLn "------------------------------------------------------------------"
