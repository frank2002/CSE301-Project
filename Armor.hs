module Armor
  ( Armor(..)
  , first_armor
  , armor_list
  , displayInfoArmor
  ) where

data Armor = Armor
  { armorAttackBonus :: Int
  , armorHealthBonus :: Int
  , armorDefense     :: Int
  , armorName        :: String
  } deriving (Show, Eq)

first_armor = Armor 5 20 5 "Default Armor"

iron_armor = Armor 7 32 18 "Iron Armor"

steel_armor = Armor 14 53 28 "Steel Armor"

mithril_armor = Armor 19 67 33 "Mithril Armor"

dragon_scale = Armor 26 78 42 "Dragon Scale Armor"

shadow_cloak = Armor 35 87 52 "Shadow Cloak"

celestial_plate = Armor 49 104 68 "Celestial Plate"

demon_hide = Armor 53 133 78 "Demon Hide Armor"

guardian_mail = Armor 69 182 88 "Guardian Mail"

aegis_shield = Armor 74 201 97 "Aegis Shield"

titanium_vest = Armor 99 253 108 "Titanium Vest"

armor_list =
  [ iron_armor
  , steel_armor
  , mithril_armor
  , dragon_scale
  , shadow_cloak
  , celestial_plate
  , demon_hide
  , guardian_mail
  , aegis_shield
  , titanium_vest
  ]


--------------------------------------------------------------------
-- Display information of Armor
--------------------------------------------------------------------
displayInfoArmor :: Armor -> IO ()
displayInfoArmor armor = do
  putStrLn
    $ "{"
        ++ show (armorName armor)
        ++ ": Attack Bonus: "
        ++ show (armorAttackBonus armor)
        ++ ", Health Bonus: "
        ++ show (armorHealthBonus armor)
        ++ ", Defense: "
        ++ show (armorDefense armor)
        ++ "}"
