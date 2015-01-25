{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}
{-# OPTIONS -Wall #-}

module Language.Hakaru.Any (Any(Any, unAny), Any') where

import Language.Hakaru.Syntax (Lambda, Mochastic, Integrate)
import Data.Typeable (Typeable)

newtype Any a = Any { unAny :: Any' a }
  deriving Typeable
  -- beware GHC 7.8 https://ghc.haskell.org/trac/ghc/wiki/GhcKinds/PolyTypeable

type Any' a =
  forall repr. (Mochastic repr, Integrate repr, Lambda repr) => repr a
