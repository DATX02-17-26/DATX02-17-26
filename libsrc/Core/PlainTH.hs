{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Core.PlainTH where

import Language.Haskell.TH

stdDerive :: Name -> Name -> Name -> DecQ
stdDerive constr clazz typ = do
  p <- varT <$> newName "p"
  let tconstr = pure $ ConT constr
  let tclazz  = pure $ ConT clazz
  let ttyp    = pure $ ConT typ
  standaloneDerivD (cxt [[t|$tconstr $tclazz $p|]])
                   [t| $tclazz ($ttyp $p) |]