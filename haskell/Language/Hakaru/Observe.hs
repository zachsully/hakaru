{-# LANGUAGE DataKinds
           , GADTs
           , OverloadedStrings
           , FlexibleContexts
           #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2015.12.15
-- |
-- Module      :  Language.Hakaru.Observe
-- Copyright   :  Copyright (c) 2015 the Hakaru team
-- License     :  BSD3
-- Maintainer  :  ppaml@indiana.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- A simpler version of the work done in 'Language.Hakaru.Disintegrate'
--
-- In principle, this module's functionality is entirely subsumed
-- by the work done in Language.Hakaru.Disintegrate, so we can hope
-- to define observe in terms of disintegrate. This is still useful
-- as a guide to those that want something more in line with what other
-- probabilisitc programming systems support.
----------------------------------------------------------------
module Language.Hakaru.Observe where

import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Types.HClasses
import qualified Language.Hakaru.Syntax.Prelude as P

observe
    :: (ABT Term abt)
    => abt '[] ('HMeasure a)
    -> abt '[] a 
    -> abt '[] ('HMeasure a)
observe m a = observeAST (LC_ m) (LC_ a)

observeAST
    :: (ABT Term abt)
    => LC_ abt ('HMeasure a)
    -> LC_ abt a
    -> abt '[] ('HMeasure a)
observeAST (LC_ m) (LC_ a) =
    caseVarSyn m observeVar $ \ast ->
        case ast of
        -- TODO: Add a name supply
        Let_  :$ e1 :* e2 :* End ->
            syn (Let_ :$ e1 :*
                (caseBind e2 $ \x e2' ->
                          let x' = Variable
                                     ""
                                     (nextFree m `max` nextBind m)
                                     (varType x)
                          in bind x' $ observe (rename x x' e2') a) :*
                End)
        --Dirac :$ e  :* End       -> P.if_ (e P.== a) (P.dirac a) P.reject
        -- TODO: Add a name supply
        MBind :$ e1 :* e2 :* End ->
             syn (MBind :$ e1 :*
                 (caseBind e2 $ \x e2' ->
                           let x' = Variable
                                      ""
                                      (nextFree m `max` nextBind m)
                                      (varType x)
                           in bind x' $ observe (rename x x' e2') a) :*
                 End)
        MeasureOp_ op :$ es      -> observeMeasureOp op es a
        _ -> error "observe can only be applied to measure primitives"

-- This function can't inspect a variable due to
-- calls to subst that happens in Let_ and Bind_
observeVar = error "observe can only be applied measure primitives"

observeMeasureOp
    :: (ABT Term abt, typs ~ UnLCs args, args ~ LCs typs)
    => MeasureOp typs a
    -> SArgs abt args
    -> abt '[] a
    -> abt '[] ('HMeasure a)
observeMeasureOp Normal  (mu :* sd :* End) x =
    P.withWeight
        (P.exp (P.negate (x P.- mu) P.^ P.nat_ 2
        P./ P.fromProb (P.prob_ 2 P.* sd P.^ (P.nat_ 2)))
        P./ sd
        P./ P.sqrt (P.prob_ 2 P.* P.pi)) (P.dirac x)
observeMeasureOp Uniform (lo :* hi :* End) x =
    P.if_ (lo P.<= x P.&& x P.<= hi)
          (P.withWeight (P.unsafeProb $ P.recip $ hi P.- lo) (P.dirac x))
          P.reject
observeMeasureOp (Plate t) (e1 :* End) x =
    caseVarSyn e1 observeVar $ \ast ->
      case ast of
        Array_ n e1 -> caseBind e1 $ \i e1' ->
          syn $ MeasureOp_ (Plate t)
           :$ (syn $ Array_ n $ bind i $ observe e1' (syn $ ArrayOp_  (Index t) :$ x :* (var i) :* End))
           :* End
        _ -> error "TODO other cases"
observeMeasureOp _ _ _ = error "TODO{Observe:observeMeasureOp}"
