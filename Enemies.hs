module Enemies
(
  Enemy(..)
  ,deductHealthFromEnemy
  ,enemyIsDead
  ,parademon
  ,goblin
  ,displayInfoEnemy
  ,tinyMinotaur
  ,apokoliptian
  ,orc
  ,largeMinotaur
  ,minotaurKing
  ,iceDragon
  ,darkload
  ,rice
  ,fireAmorload
  ,theGodKing
  ,deathless
  ,kingGuarder
) where

data Enemy = Enemy {
    healthPoints :: Int,
    attackPower :: Int,
    speed :: Int,
    defense :: Int,
    name :: String
} deriving (Show, Eq)

parademon = Enemy 100 5 20 10 "Parademon"

goblin = Enemy 50 10 30 10 "Goblin"

tinyMinotaur = Enemy 100 20 10 10 "Tiny Minotaur"

apokoliptian = Enemy 150 30 20 20 "Apokoliptian"

orc = Enemy 200 40 20 30 "Orc"

largeMinotaur = Enemy 400 50 10 60 "Large Minotaur"

minotaurKing = Enemy 500 60 10 70 "Minotaur King"

iceDragon = Enemy 1000 100 10 100 "Ice Dragon"

darkload = Enemy 400 40 10 60 "Darkload"

kingGuarder = Enemy 1000 80 10 50 "King Guarder"

rice = Enemy 1000 70 10 30 "Rice"

fireAmorload = Enemy 1000 100 10 100 "Fire Amorload"

theGodKing = Enemy 1500 100 10 100 "The God King"

deathless = Enemy 1000 200 10 100 "Deathless"




deductHealthFromEnemy::Enemy->Int->Enemy
deductHealthFromEnemy enemy deduction=Enemy ((healthPoints enemy)-deduction) (attackPower enemy) (speed enemy) (defense enemy) (name enemy)

enemyIsDead::Enemy->Bool
enemyIsDead enemy=(healthPoints enemy)<=0

--------------------------------------------------------------------
-- Display information of Enemy
--------------------------------------------------------------------
displayInfoEnemy :: Enemy -> IO ()
displayInfoEnemy enemy = do
    putStrLn $ "------------------------- " ++ name enemy ++ " Stats -------------------------"
    putStrLn $ "Attack Power: " ++ show (attackPower enemy) ++ ", Health Points: " ++ show (healthPoints enemy) ++ ", Speed: " ++ show (speed enemy) ++ ", Defense: " ++ show (defense enemy)