
module Observable.Examples (
    betaBinomial
  , linearFit
  , sinusoidal
  , sinusoidalModel
  ) where

import Observable.Core
import Observable.Distribution

-- | A simple beta-binomial model for testing.
betaBinomial :: Int -> Double -> Double -> Observable Int
betaBinomial n a b = do
  p <- observe "p" (beta a b)
  observe "x" (binomial n p)

-- | An example Bayesian linear regression model.
linearFit :: Double -> Double -> [Double] -> Observable [Double]
linearFit c d xs = do
  a <- observe "intercept" standard
  b <- observe "slope" standard
  v <- observe "variance" (gamma c d)
  let model x = a + b * x
  observe "ys" (isoGauss (fmap model xs) (sqrt v))

-- | An example Bayesian sinusoidal regression model.
sinusoidal :: [Double] -> Observable [Double]
sinusoidal xs = do
  a <- observe "cosParam" (normal 0 10)
  b <- observe "sinParam" (normal 0 10)
  v <- observe "variance" (invGamma 1 2)
  let model x =  a*cos x + b*sin x
  observe "ys" (isoGauss (fmap model xs) (sqrt v))

-- | The sinusoidal model prior, separated.
prior :: Observable (Double, Double, Double)
prior = do
  a <- observe "cosParam" (normal 0 10)
  b <- observe "sinParam" (normal 0 10)
  v <- observe "variance" (invGamma 1 2)
  return (a, b, v)

-- | The sinusoidal model likelihood, separated.
likelihood :: [Double] -> (Double, Double, Double) -> Observable [Double]
likelihood xs (a, b, v) = do
  let model x = a*cos x + b*sin x
  observe "ys" (isoGauss (fmap model xs) (sqrt v))

-- | An alternative spec for the sinusoidal model.
sinusoidalModel :: [Double] -> Observable [Double]
sinusoidalModel xs = prior >>= likelihood xs

