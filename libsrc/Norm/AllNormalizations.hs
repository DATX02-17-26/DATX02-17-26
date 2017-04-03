module Norm.AllNormalizations where

import CoreS.AST
import NormalizationStrategies
import Norm.NormFor
import AlphaR

normalizations :: Normalizer CompilationUnit
normalizations = [ alphaRenaming, normForToWhile ]
