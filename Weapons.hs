module Weapons
  ( Weapon(..)
  , first_weapon
  , weapon_list
  , displayInfoWeapon
  ) where

data Weapon = Weapon
  { weaponDamage    :: Int
  , weaponHitChance :: Float
  , weaponName      :: String
  } deriving (Show, Eq)

first_weapon = Weapon 5 0.6 "Default Weapon"

steel_sword = Weapon 10 0.6 "Steel Sword"

dawn_blade = Weapon 15 0.8 "Dawn Blade"

marrow = Weapon 20 0.9 "Marrow"

unfaithful = Weapon 25 1.0 "Unfaithful"

echo = Weapon 50 0.7 "Echo"

soul = Weapon 60 0.8 "Soul"

randor = Weapon 70 0.9 "Randor"

grayson = Weapon 80 1.0 "Grayson"

excalibur = Weapon 100 0.9 "Excalibur"

infinity_blade = Weapon 130 1.0 "Infinity Blade"

weapon_list =
  [ steel_sword
  , dawn_blade
  , marrow
  , unfaithful
  , echo
  , soul
  , randor
  , grayson
  , excalibur
  , infinity_blade
  ]


--------------------------------------------------------------------
-- Display information of Weapon
--------------------------------------------------------------------
displayInfoWeapon :: Weapon -> IO ()
displayInfoWeapon weapon = do
  putStrLn
    $ "{"
        ++ show (weaponName weapon)
        ++ ": Damage: "
        ++ show (weaponDamage weapon)
        ++ ", Hit Chance: "
        ++ show (weaponHitChance weapon)
        ++ "}"
