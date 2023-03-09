{-|
Module      : XCS.Conf
Description : XCS configuration
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Configuration for XCS: Learning parameters, exploration rate and termination
criterion.
-}


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}


module XCS.Conf where


import CustomPrelude


{-|
The XCS configuration is a simple product type.

Access to the fields is provided by the corresponding lenses.
-}
data Conf
  = Conf
  { _beta :: Real
    -- ^ The learning rate for 'XCS.Classifier._p', 'XCS.Classifier._e' and
    -- 'XCS.Classifier._f' (@0 <= beta <= 1@).
  , _alpha :: Real
    -- ^ Discount factor for the single classifier accuracy (@0 < alpha <= 1@).
  , _epsilon0 :: Real
    -- ^ Target error (@0 < epsilon0 <= 1@).
  , _nu :: Real
    -- ^ The power parameter (@0 < nu@); determines the steepness of the single
    -- classifier accuracy measure.
  , _gamma :: Real
    -- ^ The discount factor for future rewards (used in updating classifier
    -- predictions in multistep problems, @0 <= gamma <= 1@).

  , _nMicroMax :: Positive
    -- ^ The maximum size of the population (in micro-classifiers, i.e., it's
    -- the maximum sum of the classifiers' numerosities).
  , _thetaDel :: NonNegative
    -- ^ Deletion threshold (If the experience of a classifier is greater than
    -- this, its fitness may be considered when determining its deletion
    -- probability).
  , _delta :: Real
    -- ^ Fraction of mean fitness threshold (@0 <= delta <= 1@). If the
    -- classifier's fitness is below this times the mean fitness of action sets
    -- the classifier occurs in, the classifier's fitness may be considered when
    -- determining its deletion probability.

  , _thetaMNA :: Positive
  -- ^ Minimal number of actions threshold (if only that many different actions
  -- are proposed by the population, generate new classifiers using covering).
  , _pWild :: Probability
  -- ^ During covering, this is the probability of a gene being initialized with
  -- a higher generality.
  , _pI, _eI, _fI :: Real
  -- ^ Used as initial values for 'XCS.Classifier._p', 'XCS.Classifier._e' and
  -- 'XCS.Classifier._f' in new classifiers.

  , _thetaGA :: NonNegative
  -- ^ GA activation threshold (GA is applied in a set when average time since
  -- last time in the set is greater than this).
  , _chi :: Probability
  -- ^ Probability of GA applying crossover at all.
  , _mu :: Probability
  -- ^ During GA application, this is the probability of a gene being mutated.

  , _thetaSub :: NonNegative
  -- ^ Subsumption threshold (a classifier's experience must be greater than
  -- this in order to be able to subsume another classifier).
  , _doGASubsumption :: Bool
  -- ^ Whether to test offspring for possible logical subsumption by parents.
  , _doASetSubsumption :: Bool
  -- ^ Whether to test action sets for subsuming classifiers.

  , _pExplr :: Probability
  -- ^ Probability during action selection of choosing the action uniform
  -- randomly.
  , _terminateWhen :: NonNegative -> Bool
  -- ^ Whether, given the number of episodes already run, XCS should terminate.
  -- TODO I would like to `State stor c a -> Bool`
  -- TODO I would like to use a window of populations or rewards here etc.
  }
makeLenses ''Conf


instance Arbitrary Conf where
  arbitrary = Conf <$>
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- beta
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- alpha
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- epsilon0
      arbitrary `suchThat` (0 <) <*> -- nu
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- gamma

      arbitrary <*> -- nMicroMax
      arbitrary <*> -- thetaDel
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- delta

      arbitrary <*> -- thetaMNA
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- pWild
      arbitrary <*> -- pI
      arbitrary `suchThat` (0 <=) <*> -- eI
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- fI

      arbitrary <*> -- thetaGA
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- chi
      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- mu

      arbitrary <*> -- thetaSub
      arbitrary <*> -- doGASubsumption
      arbitrary <*> -- doASetSubsumption

      arbitrary `suchThat` (0 <=) `suchThat` (<= 1) <*> -- pExplr
      arbitrary -- terminateWhen


-- TODO proper arbitrary for terminateWhen


{-|
Default—or rather, starting point—configuration as advised by the algoritmic
description of XCS (Butz and Wilson, 2001).
-}
butz2001
  :: Positive
  -- ^ Maximum population size. Should be chosen such that “covering only occurs
  -- at the very beginning of a run” (Butz and Wilson, 2001).
  -> Real
  -- ^ Target error. “A typical value for 'epsilon0' is one percent of the
  -- maximum value of the prediction” (Butz and Wilson, 2001).
  -> Positive
  -- ^ Minimal number of actions threshold. “To cause covering to provide
  -- classifiers for every action, choose 'thetaMNA' equal to the number of
  -- available actions” (Butz and Wilson, 2001).
  -> (NonNegative -> Bool)
  -- ^ Termination condition.
  -> Conf
butz2001 nMicroMax epsilon0 thetaMNA terminateWhen = Conf
  { _beta = 0.15
  , _alpha = 0.1
  , _epsilon0 = epsilon0
  , _nu = 5
  , _gamma = 0.71

  , _nMicroMax = nMicroMax
  , _thetaDel = nonNegative 20
  , _delta = 0.1

  , _thetaMNA = thetaMNA
  , _pWild = 0.33
  , _pI = 0.01
  , _eI = 0.01
  , _fI = 0.01

  , _thetaGA = nonNegative 37
  , _chi = 0.75
  , _mu = 0.03

  , _thetaSub = nonNegative 20
  , _doGASubsumption = False
  , _doASetSubsumption = False

  , _pExplr = 0.5
  , _terminateWhen = terminateWhen
  }
