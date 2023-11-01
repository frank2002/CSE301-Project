module Enemies
(
  Enemy(..)
  ,deductHealthFromEnemy
  ,enemyIsDead
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

deductHealthFromEnemy::Enemy->Int->Enemy
deductHealthFromEnemy enemy deduction=Enemy ((healthPoints enemy)-deduction) (attackPower enemy) (speed enemy) (defense enemy) (name enemy)

enemyIsDead::Enemy->Bool
enemyIsDead enemy=(healthPoints enemy)<=0