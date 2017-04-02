module Norm.DoWToWhile

import Util.Monad (traverseJ)
import Norm.NormCS

stage :: Int
stage = 5

normDoWToWhile :: NormCUR
normDoWToWhile = makeRule' "dowtowhile.stmt.do_while_to_while" [stage]
                            execDoWToWhile

execDoWToWhile :: NormCUA
execEmptyBlock = normEvery $ \case
  Do expr stmt -> change $ traverseJ [stmt, While expr stmt]
  x                 -> unique [x]
