module Enemies
  ( Enemy(..)
  , deductHealthFromEnemy
  , enemyIsDead
  , parademon
  , goblin
  , displayInfoEnemy
  , tinyMinotaur
  , apokoliptian
  , orc
  , largeMinotaur
  , minotaurKing
  , iceDragon
  , darkload
  , rice
  , fireAmorload
  , theGodKing
  , deathless
  , kingGuarder
  ) where

data Enemy = Enemy
  { healthPoints :: Int
  , attackPower  :: Int
  , speed        :: Int
  , defense      :: Int
  , name         :: String
  } deriving (Show, Eq)

parademon = Enemy 100 15 20 10 "Parademon"

goblin = Enemy 50 10 30 10 "Goblin"

tinyMinotaur = Enemy 100 20 10 10 "Tiny Minotaur"

apokoliptian = Enemy 150 30 20 20 "Apokoliptian"

orc = Enemy 150 40 20 30 "Orc"

largeMinotaur = Enemy 300 45 10 45 "Large Minotaur"

minotaurKing = Enemy 400 50 10 55 "Minotaur King"

iceDragon = Enemy 1000 40 10 50 "Ice Dragon"

darkload = Enemy 400 40 10 40 "Darkload"

kingGuarder = Enemy 500 50 10 50 "King Guarder"

rice = Enemy 600 60 10 30 "Rice"

fireAmorload = Enemy 1000 30 10 50 "Fire Amorload"

theGodKing = Enemy 1500 70 10 60 "The God King"

deathless = Enemy 500 100 10 40 "Deathless"

deductHealthFromEnemy :: Enemy -> Int -> Enemy
deductHealthFromEnemy enemy deduction =
  Enemy
    ((healthPoints enemy) - deduction)
    (attackPower enemy)
    (speed enemy)
    (defense enemy)
    (name enemy)

enemyIsDead :: Enemy -> Bool
enemyIsDead enemy = (healthPoints enemy) <= 0

--------------------------------------------------------------------
-- Display information of Enemy
--------------------------------------------------------------------
displayInfoEnemy :: Enemy -> IO ()
displayInfoEnemy enemy = do
  putStrLn
    $ "------------------------- "
        ++ name enemy
        ++ " Stats -------------------------"
  putStrLn
    $ "Attack Power: "
        ++ show (attackPower enemy)
        ++ ", Health Points: "
        ++ show (healthPoints enemy)
        ++ ", Speed: "
        ++ show (speed enemy)
        ++ ", Defense: "
        ++ show (defense enemy)
