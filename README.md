# CSE301 - Functional Programming Project - Infinite Blade

This is a project for CSE301 - Functional Programming course at Ecole Polytechnique. The project is a game called **Infinite Blade** (We've borrowed some settings from the Epic Games-developed ["Infinity Blade"](https://en.wikipedia.org/wiki/Infinity_Blade)), which is a text adventure game based on the tree structure written in **Haskell**. 

### Group Members

---

[Junyuan Wang](https://github.com/frank2002/CSE301-Project/tree/main) - Tree structure building, game logic building, front-end design, Parser.

[Yubo Cai](https://github.com/yubocai-poly) - Data structure design (*Hero, Equipment, Enemy*), Battle system, front-end design.

[He Yang](https://github.com/yhecb) - Parser.

### Game Description

---

In the primary portion of **Infinity Blade**, the player character travels a tree labyrinth through a ruined castle and fights one-on-one battles with oversized enemies. The ultimate goal of the game is to defeat the deathless, get the Infinity Gauntlet and get out of the labyrinth. The game has a special battle system and numerical system, as well as an equipment collection system and maze decryption.

### How to Play
---
There are two ways to play the game:
```bash
# 1. Run the program with GHC with the binary file we provided
cd dictionary_on_your_laptop   # change the path to the dictionary where you put the binary file
./main
```
Or you can run the program with GHCi:
```bash
$ ghci
GHCi, version 9.6.2: https://www.haskell.org/ghc/  :? for help
Prelude> :l Main
Prelude> main
```

## GamePlay
### 1. Numerical System
Players (Hero) have 4 parameters, which are Health Points, Attack Power, Defence and Speed. And there are three types of equipment, weapons, armour and shoes.
```haskell
------------------------- Hero Data Structure -------------------------
data Hero = Hero {
    baseHealthPoints :: Int,
    baseAttackPower :: Int,
    baseSpeed :: Int,
    baseDefense :: Int,
    currentWeapon :: Weapon,
    currentArmor :: Armor,
    currentShoes :: Shoe
} deriving (Show, Eq)

------------------------- Equipments Data Structure -------------------------
data Armor = Armor {armorAttackBonus :: Int, armorHealthBonus :: Int, armorDefense :: Int, armorName :: String } deriving (Show,Eq)

data Shoe = Shoe {shoeAttackBonus :: Int, shoeHealthBonus :: Int, shoeDefense :: Int, shoeSpeed::Int, shoeName :: String } deriving (Show,Eq)

data Weapon = Weapon {weaponDamage:: Int,weaponHitChance :: Float, weaponName :: String } deriving (Show,Eq)
```
Enemies have same 4 parameters: Health Points, Attack Power, Defence and Speed.
```haskell
------------------------- Enemy Data Structure -------------------------
data Enemy = Enemy {
    healthPoints :: Int,
    attackPower :: Int,
    speed :: Int,
    defense :: Int,
    name :: String
} deriving (Show, Eq)
``` 
### 2. Battle System
The battle system is based on the numerical system. The battle is turn-based. The player and the enemy take turns to attack each other. Each attack will determine if it was successful or not for both player and enemy. The hit chance is calculated by the following formula:

$$
\text{Hero Hit Change} = \frac{\text{Weanpon Hit Change} * 100}{100 + \text{Enemy's Speed}} * 100 \%
$$

$$
\text{Enemy Hit Change} = \frac{100}{100 + \text{Hero's Speed}} * 100 \%
$$

The damage is calculated by the following formula:

$$
\text{Damage} = \frac{100 * \text{Attack Power}}{100 + \text{opponents' Defense}}
$$

### 3. Command System and Maps
We have design the [**Fog of War**](https://en.wikipedia.org/wiki/Fog_of_war) system of the map. The map displays information about places explored and areas that are in the player's immediate vicinity, such as the following example:
```
Entrance
|
`- First_trial (Enemy Defeated)
   |
   +- Apokoliptian's Room (Enemy Defeated)
   |  |
   |  +- Treasure Room
   |  |
   |  `- Orc's Room (Enemy Here!) <--- you are here!
   |     |
   |     +- Treasure Room (Unexplored)
   |     |
   |     `- MinotaurKing's Room (Unexplored) (Enemy Here!)
   |
   +- Parademon's Room (Unexplored) (Enemy Here!)
   |
   `- Empty Room (Unexplored)
```
We convert natural language into commands via the Parser program. Here are the commands we support:

1. `go down to child one/two/three` - go down to the first/second/third child of the current node.

2. `go up` - go up to the parent of the current node.

3. `check` - check the current status of you and your enemy.

4. `search` - search for equipment in the current node.

5. `battle` - battle the enemy in the current node.

6. `quit` - quit the game.