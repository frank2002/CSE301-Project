module Armor
(
  Armor(..)
  ,first_armor
  ,armor_list
) where

data Armor = Armor {armorAttackBonus :: Int, armorHealthBonus :: Int, armorDefense :: Int, armorName :: String } deriving (Show,Eq)
first_armor = Armor 5 5 5 "Fists Armor"

iron_armor = Armor 7 12 18 "Iron Armor"
steel_armor = Armor 11 23 28 "Steel Armor"
mithril_armor = Armor 14 27 33 "Mithril Armor"
dragon_scale = Armor 22 38 42 "Dragon Scale Armor"
shadow_cloak = Armor 26 47 52 "Shadow Cloak"
celestial_plate = Armor 29 64 68 "Celestial Plate"
demon_hide = Armor 33 73 78 "Demon Hide Armor"
guardian_mail = Armor 39 82 88 "Guardian Mail"
aegis_shield = Armor 44 91 97 "Aegis Shield"
titanium_vest = Armor 49 103 108 "Titanium Vest"

armor_list = [iron_armor, steel_armor, mithril_armor, dragon_scale, shadow_cloak, celestial_plate, demon_hide, guardian_mail, aegis_shield, titanium_vest]
