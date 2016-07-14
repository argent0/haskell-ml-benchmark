{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}

module Main (main) where

--import Naturals

import System.Random

data Nat :: * where
   Z :: Nat
   S :: Nat -> Nat

data SNat :: Nat -> * where
   SZ :: SNat 'Z
   SS :: SNat n -> SNat ('S n)

infixr 5 :-
data Vector :: Nat -> * -> * where
   Nil :: Vector 'Z a
   (:-) :: a -> Vector n a -> Vector ('S n) a

instance Show a => Show (Vector n a) where
   show Nil = "Nil"
   show (x :- xs) = show x ++ " :- " ++ show xs

zeros :: Num a => SNat n -> Vector n a
zeros SZ = Nil
zeros (SS s) = 0 :- zeros s

-- Inner product
{-# INLINE (%.) #-}
(%.) :: Num a => Vector ('S n) a -> Vector ('S n) a -> a
(%.) (a :- Nil) (b :- Nil) = a * b
(%.) (a :- as@(_ :- _)) (b :- bs@(_ :- _)) = a * b + (as %. bs)

type NFeatures = ('S ('S 'Z))

sNFeatures :: SNat NFeatures
sNFeatures = SS (SS SZ)

type Model = Vector ('S NFeatures) Double
type Target = Double
type Features = Vector NFeatures Double

type Hypothesis = Model -> Features -> Target

-- General purpose example
data Example = Example
   Features
   Target deriving Show

{-# INLINE nextModel #-}
nextModel :: Num a =>
   a -> --lambda
   a -> --learningRate
   a -> --difference
   Vector ('S n) a -> --model
   Vector n a -> --Features
   Vector ('S n) a --the resulting model

nextModel lambda learningRate difference (m :- model) features =
   (m - learningRate * difference) :- nextModel' lambda learningRate difference model features

nextModel' :: Num a => a -> a -> a -> Vector n a -> Vector n a -> Vector n a
nextModel' _ _ _ Nil Nil = Nil
nextModel' lambda learningRate difference (m :- ms) (f :- fs) =
   (m - learningRate * (difference * f + lambda * m)) :- nextModel' lambda learningRate difference ms fs

stochaticGradientDescentUpdate :: Hypothesis -> Double -> Double -> Example -> Model -> (Target, Model)
stochaticGradientDescentUpdate hypothesis lambda learningRate (Example features target) model = 
   let
      cost = negate (log approximation*target + log(1 - approximation)*(1-target))
      grad = nextModel lambda learningRate difference model features
   in ((,) $! cost) $! grad
   where
   difference :: Target
   difference = approximation - target
   approximation :: Target
   approximation = hypothesis model features
{-- Octave code
h = sigmoid(theta'*X');
reg_theta = [0; theta(2:end)];

first_cost_term = (log(h)*y);
second_const_term = log(1 - h) * (1 - y);
third_cost_term = (lambda/2)*sum(reg_theta.^2);

J = ((-1)/m)*( first_cost_term + second_const_term - third_cost_term);
grad = (1/m)*( ( (h - y') * X) + (lambda) *reg_theta');
-}

-- Test main
main :: IO ()
main =
   print $ foldr folder (1000,zeroModel) $ take 1000000 d
   where
   folder ex (_, model) =
      let
         (err, nxtModel) = stochaticGradientDescentUpdate sigmoidHypothesis 0.01 0.1 ex model
      in (err, nxtModel)

   d :: [Example]
   d = example <$> randomRs (0,1) (mkStdGen 332) <*> randomRs (0,1) (mkStdGen 2132)
   example :: Double -> Double -> Example
   example x y = Example (x :- y :- Nil) (if y > 0.5 then 1 else 0) 
   zeroModel = 0 :- zeros sNFeatures

sigmoidHypothesis :: Model -> Features -> Target
sigmoidHypothesis model features =
   1 / ( 1 + exp (negate $ model %. (1 :- features)))

-- vim: expandtab
