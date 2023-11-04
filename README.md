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