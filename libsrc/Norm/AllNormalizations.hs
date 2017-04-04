module Norm.AllNormalizations where

import CoreS.AST
import NormalizationStrategies
import Norm.NormFor
import Norm.CompAssignment
import AlphaR

normalizations :: Normalizer CompilationUnit
normalizations = [ alphaRenaming, normForToWhile, normCompAss ]
