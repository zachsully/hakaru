{-# LANGUAGE DataKinds,
             FlexibleContexts,
             GADTs,
             KindSignatures #-}

----------------------------------------------------------------
--                                                    2016.11.08
-- |
-- Module      :  Language.Hakaru.Regions.InferRegions
-- Copyright   :  Copyright (c) 2016 the Hakaru team
-- License     :  BSD3
-- Maintainer  :  zsulliva@indiana.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Region inference taking a Hakaru AST to a memory region
-- annotated AST
--
----------------------------------------------------------------

module Language.Hakaru.Regions.InferRegions
  ( inferRegions
  ) where

import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Regions.AST

inferRegions
  :: ABT Term abt
  => abt '[] a
  -> RegAST (abt '[] a)
inferRegions abt = caseVarSyn abt inferRegionTerm inferRegionVar

inferRegionVar = undefined
inferRegionTerm = undefined
