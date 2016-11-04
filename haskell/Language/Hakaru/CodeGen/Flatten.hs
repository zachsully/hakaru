{-# LANGUAGE CPP,
             BangPatterns,
             DataKinds,
             FlexibleContexts,
             GADTs,
             KindSignatures,
             ScopedTypeVariables,
             RankNTypes,
             TypeOperators #-}

----------------------------------------------------------------
--                                                    2016.06.23
-- |
-- Module      :  Language.Hakaru.CodeGen.Flatten
-- Copyright   :  Copyright (c) 2016 the Hakaru team
-- License     :  BSD3
-- Maintainer  :  zsulliva@indiana.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
--   Flatten takes Hakaru ABTs and C vars and returns a CStatement
-- assigning the var to the flattened ABT.
--
----------------------------------------------------------------


module Language.Hakaru.CodeGen.Flatten
  ( flattenABT
  , flattenVar
  , flattenTerm )
  where

import Language.Hakaru.CodeGen.CodeGenMonad
import Language.Hakaru.CodeGen.CSyn
import Language.Hakaru.CodeGen.Types

import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Syntax.TypeOf (typeOf)
import Language.Hakaru.Syntax.Datum hiding (Ident)
import Language.Hakaru.Syntax.IClasses
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Types.HClasses
import Language.Hakaru.Types.Coercion
import Language.Hakaru.Types.Sing

import           Control.Monad.State.Strict
import           Control.Monad (replicateM)
import           Data.Number.Natural
import           Data.Ratio
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence      as S
import qualified Data.Foldable      as F
import qualified Data.Traversable   as T


#if __GLASGOW_HASKELL__ < 710
import           Data.Functor
#endif

import Prelude hiding (log,exp,sqrt)



--------------------------------------------------------------------------------
--                                 Top Level                                  --
--------------------------------------------------------------------------------
{-

flattening an ABT will produce a continuation that takes a CExpr representing
a location where the value of the ABT should be stored. Return type of the
the continuation is CodeGen Bool, where the computed bool is whether or not
there is a Reject inside the ABT. Therefore it is only needed when computing
mochastic values

-}

flattenABT
  :: ABT Term abt
  => abt '[] a
  -> (CExpr -> CodeGen ())
flattenABT abt = caseVarSyn abt flattenVar flattenTerm

-- note that variables will find their values in the state of the CodeGen monad
flattenVar
  :: Variable (a :: Hakaru)
  -> (CExpr -> CodeGen ())
flattenVar v = \loc ->
  do v' <- CVar <$> lookupIdent v
     putStat . CExpr . Just $ loc .=. v'

flattenTerm
  :: ABT Term abt
  => Term abt a
  -> (CExpr -> CodeGen ())
flattenTerm (NaryOp_ t s)    = flattenNAryOp t s
flattenTerm (Literal_ x)     = flattenLit x
flattenTerm (Empty_ _)       = error "TODO: flattenTerm Empty"

flattenTerm (Datum_ d)       = flattenDatum d
flattenTerm (Case_ c bs)     = flattenCase c bs

flattenTerm (Array_ s e)     = flattenArray s e

-- SCon can contain mochastic terms
flattenTerm (x :$ ys)        = flattenSCon x ys

---------------------
-- Mochastic Terms --
---------------------
flattenTerm (Reject_ _)      = \loc -> putExprStat (mdataPtrReject loc .=. (intE 0)) -- fail to draw a sample
flattenTerm (Superpose_ wes) = flattenSuperpose wes





--------------------------------------------------------------------------------
--                                  SCon                                      --
--------------------------------------------------------------------------------

flattenSCon
  :: ( ABT Term abt )
  => SCon args a
  -> SArgs abt args
  -> (CExpr -> CodeGen ())

flattenSCon Let_ =
  \(expr :* body :* End) ->
    \loc -> do
      caseBind body $ \v@(Variable _ _ typ) body'->
        do ident <- createIdent v
           declare typ ident
           flattenABT expr (CVar ident)
           flattenABT body' loc

-- Lambdas produce functions and then return a function pointer
flattenSCon Lam_ =
  \(body :* End) -> undefined
    -- \loc ->
    --   do coalesceLambda body $ \vars body' ->
    --      let varMs = foldMap11 (\v -> [mkVarDecl v =<< createIdent v]) vars
    --      in  do funcId <- genIdent' "fn"
    --             argDecls <- sequence varMs

    --             cg <- get
    --             let m       = putStat . CReturn . Just =<< flattenABT body'
    --                 (_,cg') = runState m $ cg { statements = []
    --                                           , declarations = [] }
    --             put $ cg' { statements   = statements cg
    --                       , declarations = declarations cg }

    --             extDeclare . CFunDefExt $ functionDef (typeOf body')
    --                                                   funcId
    --                                                   argDecls
    --                                                   (reverse $ declarations cg')
    --                                                   (reverse $ statements cg')
  -- do at top level
  where coalesceLambda
          :: ( ABT Term abt )
          => abt '[x] a
          -> (forall (ys :: [Hakaru]) b. List1 Variable ys -> abt '[] b -> r)
          -> r
        coalesceLambda abt k =
          caseBind abt $ \v abt' ->
            caseVarSyn abt' (const (k (Cons1 v Nil1) abt')) $ \term ->
              case term of
                (Lam_ :$ body :* End) ->
                  coalesceLambda body $ \vars abt'' -> k (Cons1 v vars) abt''
                _ -> k (Cons1 v Nil1) abt'

        mkVarDecl :: Variable (a :: Hakaru) -> Ident -> CodeGen CDecl
        mkVarDecl (Variable _ _ SInt)  = return . typeDeclaration SInt
        mkVarDecl (Variable _ _ SNat)  = return . typeDeclaration SNat
        mkVarDecl (Variable _ _ SProb) = return . typeDeclaration SProb
        mkVarDecl (Variable _ _ SReal) = return . typeDeclaration SReal
        mkVarDecl (Variable _ _ (SArray t)) = \i -> do extDeclare $ arrayStruct t
                                                       return $ arrayDeclaration t i
        mkVarDecl (Variable _ _ d@(SData _ _)) = \i -> do extDeclare $ datumStruct d
                                                          return $ datumDeclaration d i
        mkVarDecl v = error $ "flattenSCon.Lam_.mkVarDecl cannot handle vars of type " ++ show v


flattenSCon (PrimOp_ op) = flattenPrimOp op

flattenSCon (ArrayOp_ op) = flattenArrayOp op

flattenSCon (Summate _ sr) =
  \(lo :* hi :* body :* End) ->
    \loc ->
      caseBind body $ \v body' ->
        do loId <- genIdent
           hiId <- genIdent
           declare (typeOf lo) loId
           declare (typeOf hi) hiId
           let loE = CVar loId
               hiE = CVar hiId
           flattenABT lo loE
           flattenABT hi hiE

           iterI <- createIdent v
           declare SNat iterI

           accI <- genIdent' "acc"
           let semiT = sing_HSemiring sr
           declare semiT accI
           assign accI (case semiT of
                          SProb -> intE 1
                          _     -> intE 0)

           let accVar  = CVar accI
               iterVar = CVar iterI


           -- logSumExp for probabilities
           reductionCG CAddOp
                       accI
                       (iterVar .=. loE)
                       (iterVar .<. hiE)
                       (CUnary CPostIncOp iterVar) $
             do tmpId <- genIdent
                declare (typeOf body') tmpId
                let tmpE = CVar tmpId
                flattenABT body' tmpE
                putStat . CExpr . Just $ (accVar .+=. tmpE)

           putExprStat (loc .=. accVar)


flattenSCon (Product _ sr) =
  \(lo :* hi :* body :* End) ->
    \loc ->
      caseBind body $ \v body' ->
        do loId <- genIdent
           hiId <- genIdent
           declare (typeOf lo) loId
           declare (typeOf hi) hiId
           let loE = CVar loId
               hiE = CVar hiId
           flattenABT lo loE
           flattenABT hi hiE

           iterI <- createIdent v
           declare SNat iterI

           accI <- genIdent' "acc"
           let semiT = sing_HSemiring sr
           declare semiT accI
           assign accI (log (intE 0))

           let accVar  = CVar accI
               iterVar = CVar iterI


           reductionCG (case semiT of
                          SProb -> CAddOp
                          _     -> CMulOp)
                       accI
                       (iterVar .=. loE)
                       (iterVar .<. hiE)
                       (CUnary CPostIncOp iterVar) $
             do tmpId <- genIdent
                declare (typeOf body') tmpId
                let tmpE = CVar tmpId
                flattenABT body' tmpE
                putExprStat $ case semiT of
                                SProb -> CAssign CAddAssOp accVar
                                _ -> CAssign CMulAssOp accVar
                            $ tmpE

           putExprStat (loc .=. accVar)


--------------------
-- SCon Coersions --
--------------------

-- at this point, only nonrecusive coersions are implemented
flattenSCon (CoerceTo_ ctyp) =
  \(e :* End) ->
    \loc ->
       do eId <- genIdent
          let eT = typeOf e
              eE = CVar eId
          declare eT eId
          flattenABT e eE
          putExprStat . (CAssign CAssignOp loc) =<< coerceToType ctyp eT eE
  where coerceToType
          :: Coercion a b
          -> Sing (c :: Hakaru)
          -> CExpr
          -> CodeGen CExpr
        coerceToType (CCons p rest) typ =
          \e ->  primitiveCoerce p typ e >>= coerceToType rest typ
        coerceToType CNil            _  = return . id

        primitiveCoerce
          :: PrimCoercion a b
          -> Sing (c :: Hakaru)
          -> CExpr
          -> CodeGen CExpr
        primitiveCoerce (Signed HRing_Int)            SNat  = nat2int
        primitiveCoerce (Signed HRing_Real)           SProb = prob2real
        primitiveCoerce (Continuous HContinuous_Prob) SNat  = nat2prob
        primitiveCoerce (Continuous HContinuous_Real) SInt  = int2real
        primitiveCoerce (Continuous HContinuous_Real) SNat  = int2real
        primitiveCoerce a b = error $ "flattenSCon CoerceTo_: cannot preform coersion "
                                    ++ show a
                                    ++ " to "
                                    ++ show b


        -- implementing ONLY functions found in Hakaru.Syntax.AST
        nat2int,nat2prob,prob2real,int2real
          :: CExpr -> CodeGen CExpr
        nat2int   = return
        nat2prob  = \n -> do ident <- genIdent' "p"
                             declare SProb ident
                             assign ident . log1p $ n .-. (intE 1)
                             return (CVar ident)
        prob2real = \p -> do ident <- genIdent' "r"
                             declare SReal ident
                             assign ident $ (expm1 p) .+. (intE 1)
                             return (CVar ident)
        int2real  = return . CCast doubleDecl


-----------------------------------
-- SCons in the Stochastic Monad --
-----------------------------------

flattenSCon (MeasureOp_ op) = flattenMeasureOp op

flattenSCon Dirac           =
  \(e :* End) ->
    \loc ->
       do sId <- genIdent' "samp"
          declare (typeOf e) sId
          let sE = CVar sId
          flattenABT e sE
          putExprStat $ mdataPtrWeight loc .=. (floatE 0)
          putExprStat $ mdataPtrSample loc .=. sE
          putExprStat $ mdataPtrReject loc .=. (intE 1)

flattenSCon MBind           =
  \(ma :* b :* End) ->
    \loc ->
      caseBind b $ \v@(Variable _ _ typ) mb ->
        do -- first
           mId <- genIdent' "m"
           declare (typeOf ma) mId
           let mE = CVar mId
           flattenABT ma (address mE)

           -- assign that sample to var
           vId <- createIdent v
           declare typ vId
           assign vId (mdataSample mE)
           flattenABT mb loc

-- for now plats make use of a global sample
flattenSCon Plate           =
  \(size :* b :* End) ->
    \loc ->
      caseBind b $ \v@(Variable _ _ typ) body ->
        do sizeId <- genIdent' "s"
           declare SNat sizeId
           let sizeE = CVar sizeId
           flattenABT size sizeE
           putExprStat $   (arrayPtrData . mdataPtrSample $ loc)
                       .=. (CCast (mkPtrDecl . buildType $ typ)
                                  (mkUnary "malloc"
                                  (sizeE .*. (CSizeOfType . mkDecl . buildType $ typ))))



           weightId <- genIdent' "w"
           declare SProb weightId
           let weightE = CVar weightId
           assign weightId (floatE 0)

           itId <- createIdent v
           declare SNat itId
           let itE = CVar itId
               currInd  = indirect $ (CMember (mdataSample loc) (Ident "data") True) .+. itE

           sampId <- genIdent' "samp"
           declare (typeOf $ body) sampId
           let sampE = CVar sampId

           reductionCG CAddOp
                       weightId
                       (itE .=. (intE 0))
                       (itE .<. sizeE)
                       (CUnary CPostIncOp itE)
                       (do flattenABT body (address sampE)
                           putExprStat (currInd .=. (mdataSample sampE))
                           putExprStat (weightE .+=. (mdataWeight sampE)))

           putExprStat $ mdataPtrWeight loc .=. weightE
           putExprStat $ mdataPtrReject loc .=. (intE 1)


-----------------------------------
-- SCon's that arent implemented --
-----------------------------------

flattenSCon x               = \_ -> \_ -> error $ "TODO: flattenSCon: " ++ show x





--------------------------------------------------------------------------------
--                                 NaryOps                                    --
--------------------------------------------------------------------------------

flattenNAryOp :: ABT Term abt
              => NaryOp a
              -> S.Seq (abt '[] a)
              -> (CExpr -> CodeGen ())
flattenNAryOp op args =
  \loc ->
    do es <- T.forM args $ \a ->
               do aId <- genIdent
                  let aE = CVar aId
                  declare (typeOf a) aId
                  _ <- flattenABT a aE
                  return aE
       case op of
         And -> boolNaryOp op "and" es loc
         Or  -> boolNaryOp op "or"  es loc
         Xor -> boolNaryOp op "xor" es loc
         Iff -> boolNaryOp op "iff" es loc

         (Sum HSemiring_Prob) -> logSumExpCG es loc

         _ -> let opE = F.foldr (binaryOp op) (S.index es 0) (S.drop 1 es)
              in  putExprStat (loc .=. opE)


  where boolNaryOp op' str es' loc' =
          let indexOf x = CMember x (Ident "index") True
              es''      = fmap indexOf es'
              expr      = F.foldr (binaryOp op')
                                  (S.index es'' 0)
                                  (S.drop 1 es'')
          in  putExprStat ((indexOf loc') .=. expr)


--------------------------------------
-- LogSumExp for NaryOp Add [SProb] --
--------------------------------------
{-

Special for addition of probabilities we have a logSumExp. This will compute the
sum of the probabilities safely. Just adding the exp(a . prob) would make us
loose any of the safety from underflow that we got from storing prob in the log
domain

-}

-- the tree traversal is a depth first search
logSumExp :: S.Seq CExpr -> CExpr
logSumExp es = mkCompTree 0 1

  where lastIndex  = S.length es - 1

        compIndices :: Int -> Int -> CExpr -> CExpr -> CExpr
        compIndices i j = CCond ((S.index es i) .>. (S.index es j))

        mkCompTree :: Int -> Int -> CExpr
        mkCompTree i j
          | j == lastIndex = compIndices i j (logSumExp' i) (logSumExp' j)
          | otherwise      = compIndices i j
                               (mkCompTree i (succ j))
                               (mkCompTree j (succ j))

        diffExp :: Int -> Int -> CExpr
        diffExp a b = expm1 ((S.index es a) .-. (S.index es b))

        -- given the max index, produce a logSumExp expression
        logSumExp' :: Int -> CExpr
        logSumExp' 0 = S.index es 0
          .+. (log1p $ foldr (\x acc -> diffExp x 0 .+. acc)
                            (diffExp 1 0)
                            [2..S.length es - 1]
                    .+. (intE $ fromIntegral lastIndex))
        logSumExp' i = S.index es i
          .+. (log1p $ foldr (\x acc -> if i == x
                                       then acc
                                       else diffExp x i .+. acc)
                            (diffExp 0 i)
                            [1..S.length es - 1]
                    .+. (intE $ fromIntegral lastIndex))


-- | logSumExpCG creates global functions for every n-ary logSumExp function
-- this reduces code size
logSumExpCG :: S.Seq CExpr -> (CExpr -> CodeGen ())
logSumExpCG seqE =
  let size   = S.length $ seqE
      name   = "logSumExp" ++ (show size)
      funcId = Ident name
  in \loc -> do-- reset the names so that the function is the same for each arity
       cg <- get
       put (cg { freshNames = suffixes })
       argIds <- replicateM size genIdent
       let decls = fmap (typeDeclaration SProb) argIds
           vars  = fmap CVar argIds
       extDeclare . CFunDefExt $ functionDef SProb
                                             funcId
                                             decls
                                             []
                                             [CReturn . Just $ logSumExp $ S.fromList vars ]
       cg' <- get
       put (cg' { freshNames = freshNames cg })
       putExprStat $ loc .=. (CCall (CVar funcId) (F.toList seqE))


--------------------------------------------------------------------------------
--                                  Literals                                  --
--------------------------------------------------------------------------------

flattenLit
  :: Literal a
  -> (CExpr -> CodeGen ())
flattenLit lit =
  \loc ->
    case lit of
      (LNat x)  -> putExprStat $ loc .=. (intE $ fromIntegral x)
      (LInt x)  -> putExprStat $ loc .=. (intE x)
      (LReal x) -> putExprStat $ loc .=. (floatE $ fromRational x)
      (LProb x) -> let rat = fromNonNegativeRational x
                       x'  = (fromIntegral $ numerator rat)
                           / (fromIntegral $ denominator rat)
                       xE  = log1p (floatE x' .-. intE 1)
                   in putExprStat (loc .=. xE)

--------------------------------------------------------------------------------
--                                Array and ArrayOps                          --
--------------------------------------------------------------------------------


flattenArray
  :: (ABT Term abt)
  => (abt '[] 'HNat)
  -> (abt '[ 'HNat ] a)
  -> (CExpr -> CodeGen ())
flattenArray arity body =
  \loc ->
    caseBind body $ \v@(Variable _ _ typ) body' ->
      let arityE = arraySize loc
          dataE  = arrayData loc in
      do flattenABT arity arityE
         putExprStat $   dataE
                     .=. (CCast (mkPtrDecl . buildType $ typ)
                                (mkUnary "malloc"
                                  (arityE .*. (CSizeOfType . mkDecl . buildType $ typ))))

         itId  <- createIdent v
         declare SNat itId
         let itE     = CVar itId
             currInd = indirect (dataE .+. itE)

         forCG (itE .=. (intE 0))
               (itE .<. arityE)
               (CUnary CPostIncOp itE)
               (flattenABT body' currInd)


--------------
-- ArrayOps --
--------------

flattenArrayOp
  :: ( ABT Term abt
     , typs ~ UnLCs args
     , args ~ LCs typs
     )
  => ArrayOp typs a
  -> SArgs abt args
  -> (CExpr -> CodeGen ())


flattenArrayOp (Index _)  =
  \(arr :* ind :* End) ->
    \loc ->
      do arrId <- genIdent' "arr"
         indId <- genIdent
         let arrE = CVar arrId
             indE = CVar indId
         declare (typeOf arr) arrId
         declare SNat indId
         flattenABT arr arrE
         flattenABT ind indE
         let valE = indirect ((CMember arrE (Ident "data") True) .+. indE)
         putExprStat (loc .=. valE)

flattenArrayOp (Size _)   =
  \(arr :* End) ->
    \loc ->
      do arrId <- genIdent' "arr"
         declare (typeOf arr) arrId
         let arrE = CVar arrId
         flattenABT arr arrE
         putExprStat (loc .=. (CMember arrE (Ident "size") True))

flattenArrayOp (Reduce _) =
  \(fun :* base :* arr :* End) -> undefined
  -- do funE  <- flattenABT fun
  --    baseE <- flattenABT base
  --    arrE  <- flattenABT arr
  --    accI  <- genIdent' "acc"
  --    iterI <- genIdent' "iter"

  --    let sizeE = CMember arrE (Ident "size") True
  --        iterE = CVar iterI
  --        accE  = CVar accI
  --        cond  = iterE .<. sizeE
  --        inc   = CUnary CPostIncOp iterE

  --    declare (typeOf base) accI
  --    declare SInt iterI
  --    assign accI baseE
  --    forCG (iterE .=. (intE 0)) cond inc $
  --      assign accI $ CCall funE [accE]

  --    return accE


--------------------------------------------------------------------------------
--                                 Datum and Case                             --
--------------------------------------------------------------------------------
{-

Datum are sums of products of types. This maps to a C structure. flattenDatum
will produce a literal of some datum type. This will also produce a global
struct representing that datum which will be needed for the C compiler.

-}


flattenDatum
  :: (ABT Term abt)
  => Datum (abt '[]) (HData' a)
  -> (CExpr -> CodeGen ())
flattenDatum (Datum _ typ code) =
  \loc ->
    do extDeclare $ datumStruct typ
       assignDatum code loc

datumNames :: [String]
datumNames = filter (\n -> not $ elem (head n) ['0'..'9']) names
  where base = ['0'..'9'] ++ ['a'..'z']
        names = [[x] | x <- base] `mplus` (do n <- names
                                              [n++[x] | x <- base])

assignDatum
  :: (ABT Term abt)
  => DatumCode xss (abt '[]) c
  -> CExpr
  -> CodeGen ()
assignDatum code ident =
  let index     = getIndex code
      indexExpr = CMember ident (Ident "index") True
  in  do putExprStat (indexExpr .=. (intE index))
         sequence_ $ assignSum code ident
  where getIndex :: DatumCode xss b c -> Integer
        getIndex (Inl _)    = 0
        getIndex (Inr rest) = succ (getIndex rest)

assignSum
  :: (ABT Term abt)
  => DatumCode xs (abt '[]) c
  -> CExpr
  -> [CodeGen ()]
assignSum code ident = fst $ runState (assignSum' code ident) datumNames

assignSum'
  :: (ABT Term abt)
  => DatumCode xs (abt '[]) c
  -> CExpr
  -> State [String] [CodeGen ()]
assignSum' (Inr rest) topIdent =
  do (_:names) <- get
     put names
     assignSum' rest topIdent
assignSum' (Inl prod) topIdent =
  do (name:_) <- get
     return $ assignProd prod topIdent (CVar . Ident $ name)

assignProd
  :: (ABT Term abt)
  => DatumStruct xs (abt '[]) c
  -> CExpr
  -> CExpr
  -> [CodeGen ()]
assignProd dstruct topIdent sumIdent =
  fst $ runState (assignProd' dstruct topIdent sumIdent) datumNames

assignProd'
  :: (ABT Term abt)
  => DatumStruct xs (abt '[]) c
  -> CExpr
  -> CExpr
  -> State [String] [CodeGen ()]
assignProd' Done _ _ = return []
assignProd' (Et (Konst d) rest) topIdent (CVar sumIdent) =
  do (name:names) <- get
     put names
     let varName  = CMember (CMember (CMember topIdent
                                              (Ident "sum")
                                              True)
                                     sumIdent
                                     True)
                            (Ident name)
                            True
     rest' <- assignProd' rest topIdent (CVar sumIdent)
     return $ [flattenABT d varName] ++ rest'
assignProd' _ _ _  = error $ "TODO: assignProd Ident"


----------
-- Case --
----------

-- currently we can only match on boolean values
flattenCase
  :: forall abt a b k
  .  (ABT Term abt)
  => abt '[] a
  -> [Branch a abt b]
  -> (CExpr -> CodeGen ())

flattenCase c (Branch (PDatum _ (PInl PDone)) trueB:Branch (PDatum _ (PInr (PInl PDone))) falseB:[]) =
  \loc ->
    do cId <- genIdent
       declare (typeOf c) cId
       let cE = (CVar cId)
       flattenABT c cE

       cg <- get
       let trueM    = flattenABT trueB loc
           falseM   = flattenABT falseB loc
           (_,cg')  = runState trueM $ cg { statements = [] }
           (_,cg'') = runState falseM $ cg' { statements = [] }
       put $ cg'' { statements = statements cg }

       putStat $ CIf ((CMember cE (Ident "index") True) .==. (intE 0))
                     (CCompound . fmap CBlockStat . reverse . statements $ cg')
                     Nothing
       putStat $ CIf ((CMember cE (Ident "index") True) .==. (intE 1))
                     (CCompound . fmap CBlockStat . reverse . statements $ cg'')
                     Nothing


flattenCase _ _ = error "TODO: flattenCase"


--------------------------------------------------------------------------------
--                                     PrimOp                                 --
--------------------------------------------------------------------------------

flattenPrimOp
  :: ( ABT Term abt
     , typs ~ UnLCs args
     , args ~ LCs typs)
  => PrimOp typs a
  -> SArgs abt args
  -> (CExpr -> CodeGen ())


flattenPrimOp Pi =
  \End ->
    \loc -> let piE = log1p ((CVar . Ident $ "M_PI") .-. (intE 1)) in
      putExprStat (loc .=. piE)

flattenPrimOp Not =
  \(a :* End) ->
    \loc ->
      do tmpId <- genIdent' "not"
         declare sBool tmpId
         let tmpE = CVar tmpId
         flattenABT a tmpE
         let datumIndex = CMember tmpE (Ident "index") True
         putExprStat $ datumIndex .=. (CCond (datumIndex .==. (intE 1))
                                             (intE 0)
                                             (intE 1))

flattenPrimOp RealPow =
  \(base :* exponent :* End) ->
    \loc ->
      do baseId <- genIdent
         exponentId <- genIdent
         declare SProb baseId
         declare SReal exponentId
         let baseE     = CVar baseId
             exponentE = CVar exponentId
         flattenABT base baseE -- first argument is a Prob
         flattenABT exponent exponentE
         let realPow = CCall (CVar . Ident $ "pow")
                             [ expm1 baseE .+. (intE 1), exponentE]
         putExprStat $ loc .=. (log1p (realPow .-. (intE 1)))

flattenPrimOp (NatPow baseTyp) =
  \(base :* exponent :* End) ->
    \loc ->
      let sBase = sing_HSemiring baseTyp in
      do baseId <- genIdent
         exponentId <- genIdent
         declare sBase baseId
         declare SReal exponentId
         let baseE     = CVar baseId
             exponentE = CVar exponentId
         flattenABT base baseE
         flattenABT exponent exponentE
         let powerOf x y = CCall (CVar . Ident $ "pow") [x,y]
             value = case sBase of
                       SProb -> log1p $ (powerOf (expm1 baseE .+. (intE 1)) exponentE)
                                  .-. (intE 1)
                       _     -> powerOf baseE exponentE
         putExprStat $ loc .=. value

flattenPrimOp (NatRoot baseTyp) =
  \(base :* root :* End) ->
    \loc ->
      let sBase = sing_HRadical baseTyp in
      do baseId <- genIdent
         rootId <- genIdent
         declare sBase baseId
         declare SReal rootId
         let baseE = CVar baseId
             rootE = CVar rootId
         flattenABT base baseE
         flattenABT root rootE
         let powerOf x y = CCall (CVar . Ident $ "pow") [x,y]
             recipE = (floatE 1) ./. rootE  
             value = case sBase of
                       SProb -> log1p $ (powerOf (expm1 baseE .+. (intE 1)) recipE)
                                      .-. (intE 1)
                       _     -> powerOf baseE recipE
         putExprStat $ loc .=. value

flattenPrimOp (Recip t) =
  \(a :* End) ->
    \loc ->
      do aId <- genIdent
         declare (typeOf a) aId
         let aE = CVar aId
         flattenABT a aE
         case t of
           HFractional_Real -> putExprStat $ loc .=. ((intE 1) ./. aE)
           HFractional_Prob -> putExprStat $ loc .=. (CUnary CMinOp aE)

-- | exp : real -> prob, because of this we can just turn it into a prob without taking
--   its log, which would give us an exp in the log-domain
flattenPrimOp Exp = \(a :* End) -> flattenABT a

flattenPrimOp (Equal _) =
  \(a :* b :* End) ->
    \loc ->
      do aId <- genIdent
         bId <- genIdent
         let aE = CVar aId
             bE = CVar bId
             aT = typeOf a
             bT = typeOf b
         declare aT aId
         declare bT bId
         flattenABT a aE
         flattenABT b bE

         -- special case for booleans
         let aE' = case aT of
                     (SData _ (SPlus SDone (SPlus SDone SVoid))) -> (CMember aE (Ident "index") True)
                     _ -> aE
         let bE' = case bT of
                     (SData _ (SPlus SDone (SPlus SDone SVoid))) -> (CMember bE (Ident "index") True)
                     _ -> bE

         putExprStat $   (CMember loc (Ident "index") True)
                     .=. (CCond (aE' .==. bE') (intE 0) (intE 1))


flattenPrimOp (Less _) =
  \(a :* b :* End) ->
    \loc ->
      do aId <- genIdent
         bId <- genIdent
         let aE = CVar aId
             bE = CVar bId
         declare (typeOf a) aId
         declare (typeOf b) bId
         flattenABT a aE
         flattenABT b bE
         putExprStat $ (CMember loc (Ident "index") True)
                     .=. (CCond (aE .<. bE) (intE 0) (intE 1))

flattenPrimOp (Negate HRing_Real) =
 \(a :* End) ->
   \loc ->
     do negId <- genIdent' "neg"
        declare SReal negId
        let negE = CVar negId
        flattenABT a negE
        putExprStat $ loc .=. (CUnary CMinOp $ negE)


flattenPrimOp t  = \_ -> error $ "TODO: flattenPrimOp: " ++ show t


--------------------------------------------------------------------------------
--                           MeasureOps and Superpose                         --
--------------------------------------------------------------------------------

{-

The sections contains operations in the stochastic monad. See also
(Dirac, MBind, and Plate) found in SCon. Also see Reject found at the top level.

Remember in the C runtime. Measures are housed in a measure function, which
takes an `struct mdata` location. The MeasureOp attempts to store a value at
that location and returns 0 if it fails and 1 if it succeeds in that task.

-}


flattenMeasureOp
  :: ( ABT Term abt
     , typs ~ UnLCs args
     , args ~ LCs typs)
  => MeasureOp typs a
  -> SArgs abt args
  -> (CExpr -> CodeGen ())


flattenMeasureOp Uniform =
  \(a :* b :* End) ->
    \loc ->
      do aId <- genIdent
         bId <- genIdent
         let aE = CVar aId
             bE = CVar bId
         declare SReal aId
         declare SReal bId
         flattenABT a aE
         flattenABT b bE
         let r      = CCast doubleDecl rand
             rMax   = CCast doubleDecl (CVar . Ident $ "RAND_MAX")
             value  = (aE .+. ((r ./. rMax) .*. (bE .-. aE)))

         putExprStat $ mdataPtrWeight loc .=. (floatE 0)
         putExprStat $ mdataPtrSample loc .=. value
         putExprStat $ mdataPtrReject loc .=. (intE 1)


flattenMeasureOp Normal  =
  \(a :* b :* End) ->
    \loc ->
      do let randomE = (CCast doubleDecl rand)
                   ./. (CCast doubleDecl (CVar . Ident $ "RAND_MAX"))

         -- mean
         aId <- genIdent
         declare SReal aId
         let aE = CVar aId
         flattenABT a aE

         -- variance
         bId <- genIdent
         declare SProb bId
         let bE = CVar bId
         flattenABT b bE

         -- temp vars
         uId <- genIdent
         vId <- genIdent
         qId <- genIdent
         rId <- genIdent

         declare SReal uId
         declare SReal vId
         declare SReal qId
         declare SReal rId

         let vE = CVar vId
             uE = CVar uId
             qE = CVar qId
             rE = CVar rId

         doWhileCG ((qE .==. (intE 0)) .||. (qE .>. (intE 1)))
           $ do assign uId $ randomE .*. (floatE 2) .-. (floatE 1)
                assign vId $ randomE .*. (floatE 2) .-. (floatE 1)
                assign qId $ (uE .*. uE) .+. (vE .*. vE)

         assign rId $ sqrt ((mkUnary "-" (intE 2)) .*. (log qE ./. qE))
         let value  = aE .+. (uE .*. (rE .*. ((expm1 bE) .+. (intE 1))))

         putExprStat $ mdataPtrWeight loc .=. (floatE 0)
         putExprStat $ mdataPtrSample loc .=. value
         putExprStat $ mdataPtrReject loc .=. (intE 1)


flattenMeasureOp Categorical = \(arr :* End) ->
  \loc ->
    do arrId <- genIdent
       declare (typeOf arr) arrId
       let arrE = CVar arrId
       flattenABT arr arrE

       itId <- genIdent' "it"
       declare SInt itId
       let itE = CVar itId

       wSumId <- genIdent' "ws"
       declare SProb wSumId
       let wSumE = CVar wSumId
       assign wSumId (log (intE 0))

       let currE = indirect (arrayData arrE .+. itE)
           cond  = itE .<. (arraySize arrE)
           inc   = CUnary CPostIncOp itE

       isPar <- isParallel
       mkSequential

       -- first calculate the max weight
       forCG (itE .=. (intE 0)) cond inc $
         logSumExpCG (S.fromList [wSumE,currE]) wSumE

       -- draw number from uniform(0, weightSum)
       rId <- genIdent' "r"
       declare SReal rId
       let r    = CCast doubleDecl rand
           rMax = CCast doubleDecl (CVar . Ident $ "RAND_MAX")
           rE = CVar rId
       assign rId ((r ./. rMax) .*. (exp wSumE))

       assign wSumId (log (intE 0))
       assign itId (intE 0)
       whileCG (intE 1)
         $ do stat <- runCodeGenBlock $
                        do putExprStat $ mdataPtrWeight loc .=. (intE 0)
                           putExprStat $ mdataPtrSample loc .=. itE
                           putExprStat $ mdataPtrReject loc .=. (intE 1)
                           putStat CBreak
              putStat $ CIf (rE .<. (exp wSumE)) stat Nothing
              logSumExpCG (S.fromList [wSumE,currE]) wSumE
              putExprStat $ CUnary CPostIncOp itE

       when isPar mkParallel


flattenMeasureOp x = error $ "TODO: flattenMeasureOp: " ++ show x

---------------
-- Superpose --
---------------

flattenSuperpose
    :: (ABT Term abt)
    => NE.NonEmpty (abt '[] 'HProb, abt '[] ('HMeasure a))
    -> (CExpr -> CodeGen ())

-- do we need to normalize?
flattenSuperpose pairs =
  let pairs' = NE.toList pairs in

  if length pairs' == 1
  then \loc -> let (w,m) = head pairs' in
         do mId <- genIdent
            wId <- genIdent
            declare (typeOf m) mId
            declare SProb wId
            let mE = address . CVar $ mId
                wE = CVar wId
            flattenABT w wE
            flattenABT m mE
            putExprStat $ mdataPtrWeight loc .=. ((mdataPtrWeight mE) .+. wE)
            putExprStat $ mdataPtrSample loc .=. (mdataPtrSample mE)
            putExprStat $ mdataPtrReject loc .=. (mdataPtrReject mE)

  else \loc ->
         do wEs <- forM pairs' $ \(w,_) ->
                     do wId <- genIdent' "w"
                        declare SProb wId
                        let wE = CVar wId
                        flattenABT w wE
                        return wE

            wSumId <- genIdent' "ws"
            declare SProb wSumId
            let wSumE = CVar wSumId
            logSumExpCG (S.fromList wEs) wSumE

            -- draw number from uniform(0, weightSum)
            rId <- genIdent' "r"
            declare SReal rId
            let r    = CCast doubleDecl rand
                rMax = CCast doubleDecl (CVar . Ident $ "RAND_MAX")
                rE = CVar rId
            assign rId ((r ./. rMax) .*. (exp wSumE))

            -- an iterator for picking a measure
            itId <- genIdent' "it"
            declare SProb itId
            let itE = CVar itId
            assign itId (log (intE 0))

            -- an output measure to assign to
            outId <- genIdent' "out"
            declare (typeOf . snd . head $ pairs') outId
            let outE = address $ CVar outId

            outLabel <- genIdent' "exit"

            forM_ (zip wEs pairs')
              $ \(wE,(_,m)) ->
                  do logSumExpCG (S.fromList [itE,wE]) itE
                     stat <- runCodeGenBlock (flattenABT m outE >> putStat (CGoto outLabel))
                     putStat $ CIf (rE .<. (exp itE)) stat Nothing

            putStat $ CLabel outLabel (CExpr Nothing)
            putExprStat $ mdataPtrWeight loc .=. ((mdataPtrWeight outE) .+. wSumE)
            putExprStat $ mdataPtrSample loc .=. (mdataPtrSample outE)
            putExprStat $ mdataPtrReject loc .=. (mdataPtrReject outE)
