{-# LANGUAGE FlexibleContexts #-}

module Norm.VDTest where

import CoreS.AST
import CoreS.Parse
import CoreS.ConvBack
import Language.Java.Pretty

import Norm.VarDecl
import NormalizationStrategies
import AlphaR

import Control.Monad.Except
import Control.Monad.Trans.Except
import Util.Monad
import Util.Debug


import qualified Language.Java.Syntax as S

u = undefined

tn = [
     --  alphaRenaming
       normMoveForTVD
     , normSingleTVDs
     , normVDIArrLeft
     , normSplitInit
     , normVDTop
     , normSortT
     --, alphaRenaming
     ]

section h = do
  putStrLn $ replicate 80 '-'
  putStrLn h
  putStrLn $ replicate 80 '-'


fixture = [ SVars
            { _sVDecl = TypedVVDecl
              { _tvdType = VMType
                { _vmMod = VMNormal
                , _vmType = PrimT { _tPrim = IntT }
                }
              , _tvdVDecls =
                [ VarDecl
                  { _vdVDI = VarDId { _vdiIdent = Ident { _idId = "a" } }
                  , _vdVInit = Nothing
                  }
                ]
              }
            }
          , SVars
            { _sVDecl = TypedVVDecl
              { _tvdType = VMType
                { _vmMod = VMNormal
                , _vmType = PrimT { _tPrim = IntT }
                }
              , _tvdVDecls =
                [ VarDecl
                  { _vdVDI = VarDId { _vdiIdent = Ident { _idId = "b" } }
                  , _vdVInit = Nothing
                  }
                ]
              }
            }
          , SVars
            { _sVDecl = TypedVVDecl
              { _tvdType = VMType
                { _vmMod = VMNormal
                , _vmType = PrimT { _tPrim = IntT }
                }
              , _tvdVDecls =
                [ VarDecl
                  { _vdVDI = VarDId { _vdiIdent = Ident { _idId = "c" } }
                  , _vdVInit = Nothing
                  }
                ]
              }
            }
          , SVars
            { _sVDecl = TypedVVDecl
              { _tvdType = VMType
                { _vmMod = VMNormal
                , _vmType = PrimT { _tPrim = IntT }
                }
              , _tvdVDecls =
                [ VarDecl
                  { _vdVDI = VarDId { _vdiIdent = Ident { _idId = "d" } }
                  , _vdVInit = Nothing
                  }
                ]
              }
            }
          ]


stuff :: String -> IO ()
stuff cst = do
  ast <- exitLeft $ parseConv cst :: IO CompilationUnit

  let ast4 = executeNormalizer tn ast

  {-
  (ast2, u2) <- runNT $ execVDTop ast
  print u2
  section "first pass done"

  (ast3, u3) <- runNT $ execVDTop ast2
  print u3
  section "second pass done"

  (ast4, u4) <- runNT $ execVDTop ast3
  print u4
  section "third pass done"

  -}

  dumpCore ast4

test :: IO ()
test = do
  cst <- readFile "D:\\programming\\DATX02-17-26\\VarDecl.java"
  stuff cst