{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}

module Main (main) where

import System.Random
import Data.Foldable (toList, foldl')

import qualified Data.Vector as V

data Nat :: * where
   Z :: Nat
   S :: Nat -> Nat

data SNat :: Nat -> * where
   SZ :: SNat 'Z
   SS :: SNat n -> SNat ('S n)

instance Show (SNat n) where
   show = ("SNat " ++) . show . snatToInt

snatToInt :: SNat n -> Int
snatToInt = snatToInt' 0
   where
   snatToInt' :: Int -> SNat n -> Int
   snatToInt' acc SZ = acc
   snatToInt' acc (SS sn) = snatToInt' (acc+1) sn

infixr 5 :-
data Vector' :: Nat -> * -> * where
   Nil :: Vector' 'Z a
   (:-) :: a -> Vector' n a -> Vector' ('S n) a

instance Foldable (Vector' n) where
   foldMap _ Nil = mempty
   foldMap f (x :- xs) = f x `mappend` foldMap f xs

data Vector :: Nat -> * where
   Vector :: SNat n -> V.Vector Double -> Vector n

instance Show (Vector n) where
   show (Vector _ a) = "Vector " ++ show (V.length a) ++ " " ++ show a

vector :: SNat ('S n) -> Vector' ('S n) Double -> Vector ('S n)
vector sn = Vector sn . foldl' V.snoc V.empty . toList 

zeros :: SNat ('S n) -> Vector ('S n)
zeros sn = Vector sn $ V.replicate (snatToInt sn) 0

type NFeatures = ('S ('S 'Z))

sNFeatures :: SNat NFeatures
sNFeatures = SS (SS SZ)

type Model = Vector ('S NFeatures)
type Target = Double
type Features = Vector NFeatures

type Hypothesis = Model -> Features -> Target

-- General purpose example
data Example = Example
   Features
   Target deriving Show

{-# INLINE nextModel #-}
nextModel ::
   Double -> --lambda
   Double -> --learningRate
   Double -> --difference
   Model -> --model
   Features ->
   Model --the resulting model

nextModel lambda learningRate difference (Vector sn modelArr) (Vector _ featureArr) = Vector sn $
   V.update
      (V.zipWith (-) modelArr $ (* learningRate) <$> V.zipWith (+) (fmap (* difference) featureArr) (fmap (* lambda) modelArr))
      (V.singleton (0, (modelArr V.! 0) - learningRate * difference))

stochaticGradientDescentUpdate :: Hypothesis -> Double -> Double -> Example -> Model -> (Target, Model)
stochaticGradientDescentUpdate hypothesis lambda learningRate (Example features target) model = 
   let
      cost = negate (log approximation*target + log(1 - approximation)*(1-target))
      nxtModel = nextModel lambda learningRate difference model features
   in ((,) $! cost) $! nxtModel
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
   example x y =
      Example
         (vector
            sNFeatures -- SS because of the 1
            (x :- y :- Nil))
         (if y > 0.5 then 1 else 0) 
   zeroModel = zeros (SS sNFeatures)

sigmoidHypothesis :: Model -> Features -> Target
sigmoidHypothesis (Vector _ modelArr) (Vector _ featuresArr) =
   1 / ( 1 + exp (negate $ V.foldl' (+) 0 $ V.zipWith (*) modelArr (V.cons 1 featuresArr)))

-- vim: expandtab
