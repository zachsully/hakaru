{-# LANGUAGE DataKinds,
             FlexibleContexts,
             GADTs,
             KindSignatures,
             RankNTypes        #-}

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

module Language.Hakaru.CodeGen.InferRegions
  ( inferRegions
  ) where

import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Types.DataKind
import Language.Hakaru.CodeGen.RegionSyn

inferRegions
  :: ABT Term abt
  => abt '[] a
  -> RegAST (abt '[] a)
inferRegions abt = caseVarSyn abt inferRegionVar inferRegionTerm

inferRegionVar
  :: Variable (a :: Hakaru)
  -> RegAST ast
inferRegionVar = undefined

inferRegionTerm
  :: ABT Term abt
  => Term abt a
  -> RegAST (abt '[] a)
inferRegionTerm (NaryOp_ _ _) = undefined
inferRegionTerm (Literal_ _) = undefined
inferRegionTerm (Empty_ _) = undefined
inferRegionTerm (Datum_ _) = undefined
inferRegionTerm (Case_ _ _) = undefined
inferRegionTerm (Array_ _ _) = undefined
inferRegionTerm (_ :$ _) = undefined
inferRegionTerm (Reject_ _) = undefined
inferRegionTerm (Superpose_ _) = undefined
