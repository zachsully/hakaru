----------------------------------------------------------------
--                                                    2016.12.20
-- |
-- Module      :  Language.Hakaru.CodeGen.Libraries
-- Copyright   :  Copyright (c) 2016 the Hakaru team
-- License     :  BSD3
-- Maintainer  :  zsulliva@indiana.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Bindings to common C libraries
--
----------------------------------------------------------------

module Language.Hakaru.CodeGen.Libs
  ( -- math.h
    expE, expm1E, logE, log1pE, sqrtE,
    infinityE,negInfinityE,

    -- stdio.h
    printfE, sscanfE, fopenE, fcloseE, fileT, feofE, fgetsE, fgetcE, rewindE,

    -- stdlib.h
    randE, srandE, mallocE, freeE,

    -- Boehm Gargbage Collector
    gc_initE, gc_mallocE, gc_malloc_atomicE, gc_reallocE, gc_freeE
  ) where

import Language.Hakaru.CodeGen.AST

{-

  As a convention to make the CExpressions standout, functions that return CExpr
  have a suffix 'E' for instance 'printfE'

-}


--------------------------------------------------------------------------------
--                                 Lib C                                      --
--------------------------------------------------------------------------------
{-
  Here we have calls to a very small subset of functionality provided by libc.
  In the future, we should have a standard way to add in bindings to C
  libraries. Easily generating code for existing C libraries is one of the key
  design goals of pedantic-c
-}

------------
-- math.h --
------------

expE,expm1E,logE,log1pE,sqrtE :: CExpr -> CExpr
expE   = mkUnaryE "exp"
expm1E = mkUnaryE "expm1"
logE   = mkUnaryE "log"
log1pE = mkUnaryE "log1p"
sqrtE  = mkUnaryE "sqrt"

infinityE,negInfinityE :: CExpr
infinityE    = (intE 1) ./. (intE 0)
negInfinityE = logE (intE 0)

--------------
-- stdlib.h --
--------------

randE :: CExpr
randE = mkCallE "rand" []

srandE :: CExpr -> CExpr
srandE e = mkCallE "srand" [e]

mallocE :: CExpr -> CExpr
mallocE = mkUnaryE "malloc"

freeE :: CExpr -> CExpr
freeE = mkUnaryE "free"

--------------
-- stdio.h --
--------------

printfE,sscanfE :: [CExpr] -> CExpr
printfE = mkCallE "printf"
sscanfE = mkCallE "sscanf"

fopenE :: CExpr -> CExpr -> CExpr
fopenE e0 e1 = mkCallE "fopen" [e0,e1]

fcloseE,feofE,rewindE :: CExpr -> CExpr
fcloseE e = mkCallE "fclose" [e]
feofE e = mkCallE "feof" [e]
rewindE e = mkCallE "rewind" [e]

fgetsE :: CExpr -> CExpr -> CExpr -> CExpr
fgetsE e0 e1 e2 = mkCallE "fgets" [e0,e1,e2]

fgetcE :: CExpr -> CExpr
fgetcE e = mkCallE "fgetc" [e]

fileT :: CTypeSpec
fileT = CTypeDefType (Ident "FILE")

--------------------------------------------------------------------------------
--                            Boehm Garbage Collector                         --
--------------------------------------------------------------------------------
{-
   Currently needed for handling arrays and datum.

   In the future, an intermediate language based on the region calculus will be
   employed here.
-}

gc_initE :: CExpr
gc_initE = mkCallE "GC_INIT" []

gc_mallocE :: CExpr -> CExpr
gc_mallocE e = mkCallE "GC_MALLOC" [e]

gc_malloc_atomicE :: CExpr -> CExpr
gc_malloc_atomicE e = mkCallE "GC_MALLOC_ATOMIC" [e]

gc_reallocE :: CExpr -> CExpr
gc_reallocE e = mkCallE "GC_REALLOC" [e]

gc_freeE :: CExpr -> CExpr
gc_freeE e = mkCallE "GC_FREE" [e]


--------------------------------------------------------------------------------
--                                  OpenMP                                    --
--------------------------------------------------------------------------------
{-
   For generating pragmas for shared memory parallelism, that is parallelism on
   on a single process that makes use of multithreaded processors. This
   interface is implemented in most C compilers and is accessed through pragmas
-}




--------------------------------------------------------------------------------
--                                    MPI                                     --
--------------------------------------------------------------------------------
{-
   Necessary bindings to the Message Passing Inferface for distributed programs
-}
