{-# LANGUAGE DataKinds #-}

----------------------------------------------------------------
--                                                    2016.11.01
-- |
-- Module      :  Language.Hakaru.CodeGen.RegionSyn
-- Copyright   :  Copyright (c) 2016 the Hakaru team
-- License     :  BSD3
-- Maintainer  :  zsulliva@indiana.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Region Calculus for tracking memory
--
----------------------------------------------------------------


module Language.Hakaru.CodeGen.RegionSyn where

data RegAST abt a
  = Hk (abt '[] a)
  | LetRegion Region (abt '[] a)
  deriving Show
