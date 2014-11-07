{-# LANGUAGE RankNTypes #-}

module Examples.BenchmarkProblems where

import Prelude hiding (Real, not)
import Control.Monad
import Language.Hakaru.Syntax
import Language.Hakaru.Sample
import Language.Hakaru.Expect
--import qualified Language.Hakaru.Metropolis as MH
import System.Random.MWC as MWC hiding (uniform)

type Cont repr a = forall w. (a -> repr (Measure w)) -> repr (Measure w)

replicateH :: (Type a, Mochastic repr) => Int -> repr (Measure a) -> Cont repr [repr a]
replicateH 0 _ k = k []
replicateH n m k = m `bind` \x -> replicateH (n-1) m (\xs -> k (x:xs))

noisyOr :: (Mochastic repr) => repr Prob -> repr Bool_ -> repr Bool_ -> repr (Measure Bool_)
noisyOr noise x y = if_ (or_ [x, y])
                    (bern (1 - noise))
                    (bern noise)

tester prog = do
  g <- MWC.create
  replicateM 10 (unSample prog 1 g)

xor :: Base repr => repr Bool_ -> repr Bool_ -> repr Bool_
xor a b = or_ [and_ [a, not_ b], and_ [not_ a, b]] 

eq_ :: Base repr => repr Bool_ -> repr Bool_ -> repr Bool_
eq_ a b = if_ a b (not_ b)

-- Bayesian Linear Regression
linreg :: Mochastic repr => repr (Measure (Real, Real))
linreg = normal 0 2 `bind` \w1 ->
         normal 0 2 `bind` \w2 ->
         normal 0 2 `bind` \w3 ->
         normal 0 2 `bind` \w4 ->
         normal 0 2 `bind` \w5 ->
         uniform (-1) 1 `bind` \x1 ->
         normal (x1*w1 + x1*w2 + x1*w3 + x1*w4 + x1*w5) 1 `bind` \y1 ->
         uniform (-1) 1 `bind` \x2 ->
         normal (x2*w1 + x2*w2 + x2*w3 + x2*w4 + x2*w5) 1 `bind` \y2 ->
         uniform (-1) 1 `bind` \x3 ->
         normal (x3*w1 + x3*w2 + x3*w3 + x3*w4 + x3*w5) 1 `bind` \y3 ->
         dirac (pair w4 w5)

-- QMR

qmr :: Mochastic repr => repr (Measure (Bool_, Bool_))
qmr = 
 bern (1/40)   `bind` \cold ->
 bern (1/80)   `bind` \flu  ->
 bern (1/1200) `bind` \ebola ->
 noisyOr (1/10) cold flu `bind` \cough ->
 noisyOr (1/20) flu ebola `bind` \fever ->
 dirac (pair flu cold)

-- Discrete-time HMM

-- HDP-LDA

-- pCFG

-- Network Analysis

preferentialPrior :: Mochastic repr => repr (Measure Real)
preferentialPrior = uniform 0 1

numNodes          :: Mochastic repr => repr (Measure Prob)
numNodes          = poisson 5   

edgesPerNode      :: Mochastic repr => repr (Measure Prob)
edgesPerNode      = poisson 3   

-- Friends who Smoke
friendsWhoSmoke :: Mochastic repr => repr (Measure (Bool_, Bool_))
friendsWhoSmoke =
    bern (1/5) `bind` \smokes1 ->
    bern (1/5) `bind` \smokes2 ->
    bern (1/5) `bind` \smokes3 ->
    (if_ (eq_ smokes1 smokes2)
         (factor 3)
         (factor 1)) `bind_`
    dirac (pair smokes1 smokes3)

-- Seismic event monitoring

a1, b1, aN, bN, aS, bS :: Base repr => repr Prob

a1 = 20; b1 = 2
aN = 3;  bN = 2
aF = 20; bF = 1
aS = 2;  bS = 1

uV, sigV, uB, sigB, uv, sigv :: Base repr => repr Prob
uV = 5; sigV = 1
uB = 2; sigB = 1
uv = 1; sigv = 1

uT = 0; lT = 1000; aT = 20; bT = 1
uA = 0; lA = 1;    aA = 2;  bA = 1
un = 0; ln = 1;    an = 2;  bn = 1

logistic :: Base repr => repr Real -> repr Real -> repr Prob -> repr Prob
logistic x v sig = 1 / (1 + exp_ (- (x - v) / fromProb sig))

seismic :: Mochastic repr => repr (Measure Prob)
seismic = gamma a1 b1 `bind` \l0 ->
          dirac l0

-- Recursive reasoning
hiddenState :: Mochastic repr => repr (Measure Real)
hiddenState = categorical [(1, 0),
                           (1, 1),
                           (1, 2),
                           (1, 3)]

eTest :: (Integrate repr,
          Lambda repr,
          Mochastic repr) =>
         Expect repr Real -> repr (Expect' (Measure Real))
eTest n = unExpect (dirac n)

-- Lifted inference
n = 80 -- [10,20,40,80,160,320,640,1280,2560,5120]
k = 16 -- [1,2,4,8,16,32,64]

liftedInference :: Mochastic repr => repr (Measure (Bool_, Prob))
liftedInference = bern 0.01 `bind` \cause ->
                  replicateH n (if_ cause (bern 0.6) (bern 0.05))
                   (\ effects ->
                    dirac $ 
                    foldl (\ sum_ e ->
                           sum_ + (if_ e 1 0)) 0 effects) `bind` \sum_ ->
                  dirac (pair cause sum_)
