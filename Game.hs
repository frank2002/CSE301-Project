import qualified System.Random as SR
import Data.List
import Control.Monad
import qualified Control.Monad.Random as CMR
import Armor
import Weapons
import Shoes
import Hero
import Enemies
-- import Trees 
import Definition

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
  in round (fromIntegral heroAttackPower * 100 / (fromIntegral enemyDefense + 100))

enemyHitDamage :: Enemy -> Hero -> Int
enemyHitDamage enemy hero =
  let enemyDamage = attackPower enemy
      heroDefense = totalDefense hero
  in round (fromIntegral enemyDamage * 100 / (fromIntegral heroDefense + 100))

-- Simulate whether the hit is successful based on the hit rate
generateWeaponHit :: Float -> SR.StdGen -> (Bool, SR.StdGen)
generateWeaponHit hitRate gen = 
  let (randomValue, newGen) = SR.randomR (0.0, 1.0) gen  -- Generate a random Float between 0 and 1
  in (randomValue < hitRate, newGen)                   -- Return True if the random value is less than the hit rate along with the new generator

--------------------------------------------------------------------
-- Fighting program between hero and enemy
--------------------------------------------------------------------

-- This function simulates the fight between the hero and the enemy.
-- It returns a tuple containing the final state of the hero (if alive) and a list of strings representing the battle log.
fightEnemy :: Hero -> Enemy -> SR.StdGen -> Int -> IO (Maybe Hero)
fightEnemy hero enemy gen roundNumber = do
  putStrLn $ "------------------------- Round " ++ show roundNumber ++ " -------------------------"
  let heroFirst = totalSpeed hero >= speed enemy
  let (heroHit, gen') = generateWeaponHit (heroHitChance hero enemy) gen
  let (enemyHit, gen'') = generateWeaponHit (enemyHitChance enemy hero) gen'
  let enemyAfterHeroAttack = if heroFirst && heroHit
                             then deductHealthFromEnemy enemy (heroHitDamage hero enemy)
                             else enemy
  let heroAfterEnemyAttack = if not heroFirst && enemyHit
                             then deductHealth hero (enemyHitDamage enemy hero)
                             else hero
  let enemyAfterAttack = if not heroFirst && heroHit
                         then deductHealthFromEnemy enemyAfterHeroAttack (heroHitDamage hero enemy)
                         else enemyAfterHeroAttack
  let heroAfterAttack = if heroFirst && enemyHit
                        then deductHealth heroAfterEnemyAttack (enemyHitDamage enemy hero)
                        else heroAfterEnemyAttack

  putStrLn $ "Hero attacks: " ++ show heroHit
  putStrLn $ name enemy ++ " attacks: " ++ show enemyHit
  putStrLn $ "Hero HP: " ++ show (totalHealthPoints heroAfterAttack) ++ ", " ++ name enemy ++ " HP: " ++ show (healthPoints enemyAfterAttack)

  if heroIsDead heroAfterAttack then do
    putStrLn "------------------------- Fight Result -------------------------"
    putStrLn "The hero is dead."
    putStrLn $ "Total Round Number: " ++ show roundNumber
    return Nothing
  else if enemyIsDead enemyAfterAttack then do
    putStrLn "------------------------- Fight Result -------------------------"
    putStrLn $ name enemy ++ " is dead."
    putStrLn $ "Total Round Number: " ++ show roundNumber
    return (Just heroAfterAttack)
  else fightEnemy heroAfterAttack enemyAfterAttack gen'' (roundNumber + 1)

-- Helper function to print attack information
printAttack :: String -> Bool -> Hero -> Enemy -> IO ()
printAttack attacker hit hero enemy = putStrLn $ concat
  [attacker, ", Hit ", show hit, ", Hero health ", show (totalHealthPoints hero), ", Enemy Health ", show (healthPoints enemy)]

-- Initial Hero
initialHero :: Hero
initialHero = Hero {
    baseHealthPoints = 1,
    baseAttackPower = 0,
    baseSpeed = 50,
    baseDefense = 30,
    currentWeapon = first_weapon,    -- Fists initially
    currentArmor = first_armor,      -- No armor initially
    currentShoes = first_shoe        -- No shoes initially
}

hero = initialHero
goblin = Enemy 500 10 30 10 "Goblin"

--------------------------------------------------------------------
-- Testing part
--------------------------------------------------------------------

testHeroHitChance :: Float
testHeroHitChance = heroHitChance hero goblin

testEnemyHitChance :: Float
testEnemyHitChance = enemyHitChance goblin hero

testHeroHitDamage :: Int
testHeroHitDamage = heroHitDamage hero goblin

testEnemyHitDamage :: Int
testEnemyHitDamage = enemyHitDamage goblin hero

-- Test the generateWeaponHit function 100 times and count the number of True results
testGenerateWeaponHit :: Float -> SR.StdGen -> (Int, SR.StdGen)
testGenerateWeaponHit hitRate gen = runTest 100 gen 0
  where
    runTest 0 g count = (count, g)
    runTest n g count =
      let (hit, newGen) = generateWeaponHit hitRate g
      in runTest (n - 1) newGen (count + if hit then 1 else 0)

testFightEnemy :: Hero -> Enemy -> SR.StdGen -> IO ()
testFightEnemy hero enemy gen = do
  finalHeroMaybe <- fightEnemy hero enemy gen 1
  -- No need to print the total round number here, as it's already printed in fightEnemy
  case finalHeroMaybe of
    Nothing -> putStrLn "False"
    Just finalHero -> putStrLn $ "Final hero: " ++ show finalHero

-- 主函数，运行测试并打印结果
-- main :: IO ()
-- main = do
--     print testHeroHitChance
--     print testEnemyHitChance
--     print testHeroHitDamage
--     print testEnemyHitDamage
main = do
    gen <- SR.getStdGen
    testFightEnemy hero goblin gen
--   let hitRate = 0.65  -- Assuming a hit rate of 50%
--   let gen = SR.mkStdGen 42  -- Initialize the random number generator
--   let (count, _) = testGenerateWeaponHit hitRate gen  -- Run the test
--   putStrLn $ "Number of hits: " ++ show count
