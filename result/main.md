# "Haskell's stochastic gradient descent implementations comparison.

**Date:** "2016-07-14" **Author:** "Aner Oscar Lucero"

## Abstract

I made three implementations of *stochastic gradient descent* using GADTs, boxed
arrays, unboxed arrays and `Data.Vector`. I found that the GADTs implementation runs faster. I
also compared performance with python.

## Method

I made various implementations of *stochastic gradient descent* **(SGD)** to train logistic
regression models.  SGD finds the optimal parameters for the model using one
example at a time, in contrast to batch-gradient descent which uses many samples
at once to train the model. SGD only requires to define vector dot product. The
implementations differ in the way I implemented the vectors.

In pseudo-Haskell-code, SGD looks like:

```haskell
   model :: Vector (nFeatures + 1) Double
   features :: Vector (nFeatures) Double

   model := initial_model --Usually all zeros
   foreach example in examples:
      (features, target) := example
      prediction = hypotesis model features
      difference = prediction - target
      model = (model - learningRate * (difference * features + lambda * model'))

   where
   --here `dot` represents the scalar product
   hypotesis :: model -> features -> target
   hypotesis = 1 / ( 1 + exp (negate $ model `dot` (1 :- features)))
   model' = "model with it first element set to 0"
```

**GADTs**

The first implementation performs vector operations using:

```haskell
infixr 5 :-
data Vector :: Nat -> * -> * where
   Nil :: Vector 'Z a
   (:-) :: a -> Vector n a -> Vector ('S n) a

-- Inner product
{-# INLINE (%.) #-}
(%.) :: Num a => Vector ('S n) a -> Vector ('S n) a -> a
(%.) (a :- Nil) (b :- Nil) = a * b
(%.) (a :- as@(_ :- _)) (b :- bs@(_ :- _)) = a * b + (as %. bs)

```

The `foreach` part of the algorithm is handled by
```haskell
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
```

With the hypothesis calculated as:

```haskell
sigmoidHypothesis :: Model -> Features -> Target
sigmoidHypothesis model features =
   1 / ( 1 + exp (negate $ model %. (1 :- features)))
```
The whole code can be found [here](../src/MainGADT.hs).

**MonadST, STArray & IArray**

The second implementation performs vector operations using an `IArray`
parametrized by its number of elements:

```haskell
data Vector :: Nat -> * where
   Vector :: SNat n -> Int -> Array Int Double -> Vector n
```

The `foreach` part of the algorithm and the hypothesis are handled by the
following imperative-style code.

```haskell
{-# INLINE nextModel #-}
nextModel ::
   Double -> --lambda
   Double -> --learningRate
   Double -> --difference
   Model -> --model
   Features -> 
   Model --the resulting model

nextModel lambda learningRate difference (Vector sn nResultElements modelArr) (Vector _ _ featureArr) = Vector sn nResultElements $ runSTArray $ do
   result <- newArray (1, nResultElements) 0 :: ST s (STArray s Int Double)
   writeArray result 1 $ (modelArr ! 1) - learningRate * difference
   forM_ [2..nResultElements] (\elementIndex ->
      do
         let modelElement = modelArr ! elementIndex
         let featureElement = featureArr ! (elementIndex - 1)
         writeArray result elementIndex $ modelElement - learningRate * (difference * featureElement + lambda * modelElement))
   return result

sigmoidHypothesis :: Model -> Features -> Target
sigmoidHypothesis (Vector _ nElements modelArr) (Vector _ _ featuresArr) = runST $ do
   expo <- newSTRef $ modelArr ! 1
   forM_ [2..nElements] (\elementIndex ->
         modifySTRef expo (+ (modelArr ! elementIndex) * (featuresArr ! (elementIndex - 1))))
   readSTRef expo >>= \e -> return $ 1 / (1 + exp (negate e))

```

The whole code can be found [here](../src/MainST.hs).

**MonadST, STUArray & UArray**

According to the [wiki](https://wiki.haskell.org/Arrays#Unboxed_arrays), `unboxed`
arrays are a lighter version of `IArray`. It is also easy to change code using
`IArray` and `STArray` to start using `UArray` and `STUArray`.  This third
implementation does just that.

The whole code can be found [here](../src/MainSTU.hs).

**Data.Vector**

Haskell's [Data.Vector](https://hackage.haskell.org/package/vector) library is
an efficient implementation of Int-indexed arrays (both mutable and immutable),
with a powerful loop optimisation framework.

Here are the highlights of implementing SGD using this library:

```haskell
{-# INLINE nextModel #-}
import qualified Data.Vector as V

data Vector :: Nat -> * where
   Vector :: SNat n -> V.Vector Double -> Vector n

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

sigmoidHypothesis :: Model -> Features -> Target
sigmoidHypothesis (Vector _ modelArr) (Vector _ featuresArr) =
   1 / ( 1 + exp (negate $ V.foldl' (+) 0 $ V.zipWith (*) modelArr (V.cons 1 featuresArr)))
```

The whole code can be found [here](../src/MainVector.hs).

**Python**

I also made a python-3.5.2 implementation to compare. This version uses the
[`numpy`](http://www.numpy.org/)-1.11.1 python library to perform the vector operations.

The code can be found [here](../src/Main.py).

**Benchmark**

For each implementation I measured the time needed to train a model using
1,000,000 examples. This measurement is repeated 100 times to observe the
variations. The code performing this operation is found
[here](../sh/poorManBenchmarTool.sh).

All code was compiled using `-O2`.

## Results

**Comparing haskell implementations**
![plot of chunk haskell-densities](figure/haskell-densities-1.png)

* The GADT implementation is the best.

Here are some summary statistics of the running times for each Haskell
implementation.


```
##       GADT             ST             STU            VECTOR     
##  Min.   :3.256   Min.   :4.415   Min.   :4.446   Min.   :4.238  
##  1st Qu.:3.280   1st Qu.:4.474   1st Qu.:4.471   1st Qu.:4.295  
##  Median :3.296   Median :4.494   Median :4.494   Median :4.323  
##  Mean   :3.299   Mean   :4.503   Mean   :4.518   Mean   :4.330  
##  3rd Qu.:3.315   3rd Qu.:4.517   3rd Qu.:4.537   3rd Qu.:4.344  
##  Max.   :3.410   Max.   :4.742   Max.   :4.801   Max.   :4.660
```

**Comparing to (python + numpy)**

![plot of chunk haskell-python-densities](figure/haskell-python-densities-1.png)

My Haskell implementation is 9 times faster than my python one.

Here are some summary statistics of the running times for both
implementations.


```
##       GADT           PYTHON     
##  Min.   :3.256   Min.   :29.42  
##  1st Qu.:3.280   1st Qu.:29.73  
##  Median :3.296   Median :29.86  
##  Mean   :3.299   Mean   :29.91  
##  3rd Qu.:3.315   3rd Qu.:30.02  
##  Max.   :3.410   Max.   :31.03
```

## Notes

I wanted my Haskell implementations to make type-safe scalar products. The
convoluted computations of the next model are the consequence of performance
optimizations.

For the python comparison, I wanted to see how Haskell compared to a python
solution using what appears to be a popular library. In other words, I don't claim
that my python implementation is the best possible one.

All the raw data can be found in the [measurements](../measurements) folder.
