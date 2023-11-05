module Game where

import           Armor
import           Control.Monad
import           Data.List
import           Definition
import           Enemies       (Enemy (attackPower, defense, healthPoints, name, speed),
                                deductHealthFromEnemy, displayInfoEnemy,
                                enemyIsDead, goblin)
import           Hero
import           Shoes
import qualified System.Random as SR
import           Weapons

--------------------------------------------------------------------
-- Compute the hit chance of hero and enemy
--------------------------------------------------------------------
heroHitChance :: Hero -> Enemy -> Float
heroHitChance hero enemy =
  let weaponChance = weaponHitChance (currentWeapon hero)
      enemySpeed = speed enemy
   in weaponChance * 100 / (fromIntegral enemySpeed + 100)

enemyHitChance :: Enemy -> Hero -> Float
enemyHitChance enemy hero =
  let enemyChance = enemyHitChance enemy
      heroSpeed = totalSpeed hero
   in 100 / (fromIntegral heroSpeed + 150)

--------------------------------------------------------------------
-- Compute the hit chance of hero and enemy
--------------------------------------------------------------------
heroHitDamage :: Hero -> Enemy -> Int
heroHitDamage hero enemy =
  let heroAttackPower = totalAttackPower hero
      enemyDefense = defense enemy
   in round
        (fromIntegral heroAttackPower * 100 / (fromIntegral enemyDefense + 100))

enemyHitDamage :: Enemy -> Hero -> Int
enemyHitDamage enemy hero =
  let enemyDamage = attackPower enemy
      heroDefense = totalDefense hero
   in round (fromIntegral enemyDamage * 100 / (fromIntegral heroDefense + 100))

-- Simulate whether the hit is successful based on the hit rate
generateWeaponHit :: Float -> SR.StdGen -> (Bool, SR.StdGen)
generateWeaponHit hitRate gen =
  let (randomValue, newGen) = SR.randomR (0.0, 1.0) gen -- Generate a random Float between 0 and 1
   in (randomValue < hitRate, newGen) -- Return True if the random value is less than the hit rate along with the new generator

--------------------------------------------------------------------
-- Fighting program between hero and enemy
--------------------------------------------------------------------
-- This function simulates the fight between the hero and the enemy.
-- It returns a tuple containing the final state of the hero (if alive) and a list of strings representing the battle log.
fightEnemy :: Hero -> Enemy -> SR.StdGen -> Int -> IO (Maybe Hero)
fightEnemy hero enemy gen roundNumber = do
  putStrLn
    $ "------------------------- Round "
        ++ show roundNumber
        ++ " -------------------------"
  let heroFirst = totalSpeed hero >= speed enemy
  let (heroHit, gen') = generateWeaponHit (heroHitChance hero enemy) gen
  let (enemyHit, gen'') = generateWeaponHit (enemyHitChance enemy hero) gen'
  let enemyAfterHeroAttack =
        if heroFirst && heroHit
          then deductHealthFromEnemy enemy (heroHitDamage hero enemy)
          else enemy
  let heroAfterEnemyAttack =
        if not heroFirst && enemyHit
          then deductHealth hero (enemyHitDamage enemy hero)
          else hero
  let enemyAfterAttack =
        if not heroFirst && heroHit
          then deductHealthFromEnemy
                 enemyAfterHeroAttack
                 (heroHitDamage hero enemy)
          else enemyAfterHeroAttack
  let heroAfterAttack =
        if heroFirst && enemyHit
          then deductHealth heroAfterEnemyAttack (enemyHitDamage enemy hero)
          else heroAfterEnemyAttack
  putStrLn $ "Hero attacks: " ++ show heroHit
  putStrLn $ name enemy ++ " attacks: " ++ show enemyHit
  putStrLn
    $ "Hero HP: "
        ++ show (totalHealthPoints heroAfterAttack)
        ++ ", "
        ++ name enemy
        ++ " HP: "
        ++ show (healthPoints enemyAfterAttack)
  if heroIsDead heroAfterAttack
    then do
      putStrLn
        "------------------------- Fight Result -------------------------"
      putStrLn "The hero is dead."
      putStrLn $ "Total Round Number: " ++ show roundNumber
      return Nothing
    else if enemyIsDead enemyAfterAttack
           then do
             putStrLn
               "------------------------- Fight Result -------------------------"
             putStrLn $ name enemy ++ " is dead."
             putStrLn $ "Total Round Number: " ++ show roundNumber
             return (Just heroAfterAttack)
           else fightEnemy
                  heroAfterAttack
                  enemyAfterAttack
                  gen''
                  (roundNumber + 1)
