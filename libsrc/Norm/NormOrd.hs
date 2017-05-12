{- DATX02-17-26, automated assessment of imperative programs.
 - Copyright, 2017, see AUTHORS.md.
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

{-# LANGUAGE LambdaCase, TemplateHaskell, DeriveFunctor #-}

-- | 
module Norm.NormOrd where

import Norm.NormCS

import Data.Function (on)
import Data.Function.Pointless ((.:))

import Control.Monad.Writer
import Control.Monad.State  (StateT, execStateT)
import Control.Monad.Except (Except, runExcept)

import Control.Lens ((%~), (<>~), (.~), (%%~), LensLike)

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..), cons)
import Util.List (mhead, intercalate, uncons, foldl', zipRem)
import Data.Tree (Tree)
import Data.DList (DList, singleton, empty)
import Data.DTree (DTree, dtSnocL, dtSnoc, dtLeaf, toTree)

import qualified Data.Map as M
import Data.Map (Map)


import Data.Maybe (catMaybes)

import Util.Monad ((<$$>))
import Util.TH (deriveLens)

import Control.Arrow (second)

import Util.Debug

u = undefined

--------------------------------------------------------------------------------
-- Rule names:
--------------------------------------------------------------------------------

-- | A rule name. Separated with "." in output.
data RuleName = RuleName { _rname :: [String] }
  deriving (Eq, Ord, Show, Read)

-- | A root rule is a logical rule name that 
rootRule :: RuleName
rootRule = RuleName []

rnDot :: RuleName -> String
rnDot = intercalate "." . _rname

--------------------------------------------------------------------------------
-- Data types:
--------------------------------------------------------------------------------

instance Show (a -> b) where
  show _ = "function"

data Named r a = Named
  { _nMark :: a
  , _nName :: RuleName
  , _nRule :: r
  }
  deriving (Show)

type NamedRule m t = Named (NormArrT m t) ()

data DepRule r a = DepRule
  { _drRule :: Named r a
  , _drPre  :: [RuleName]
  , _drPost :: [RuleName]
  }
  deriving (Show)

data FPAlgo = FPSeq | FPSeqAdd
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type GroupRule m t a = DepRule [FPGroup m t a] a

data FPGroup m t a
  = FPGroup {
      _fpAlgo  :: FPAlgo
    , _fpRules :: GroupRule m t a
    }
  | FPUnit {
      _fpUnit :: DepRule (NormArrT m t) a
    }
  deriving (Show)

--------------------------------------------------------------------------------
-- Lens:
--------------------------------------------------------------------------------

$(deriveLens [''RuleName, ''Named, ''DepRule, ''FPAlgo, ''FPGroup])

--------------------------------------------------------------------------------
-- DSL:
--------------------------------------------------------------------------------

named :: [String] -> r -> DepRule r ()
named n r = DepRule (Named () (RuleName n) r) [] []

rule :: [String] -> NormArrT m t -> FPGroup m t ()
rule = FPUnit .: named

fpGroup :: [String] -> [FPGroup m t ()] -> FPGroup m t ()
fpGroup = FPGroup FPSeq .: named

fpAdd :: [String] -> [FPGroup m t ()] -> FPGroup m t ()
fpAdd = FPGroup FPSeqAdd .: named

class RuleDeps p where
  requires :: p -> [[String]] -> p
  breaks   :: p -> [[String]] -> p

instance RuleDeps (DepRule r a) where
  requires r p = drPre  <>~ (RuleName <$> p) $ r
  breaks   r p = drPost <>~ (RuleName <$> p) $ r

instance RuleDeps (FPGroup m t a) where
  requires  = flip $ \p -> \case
    FPGroup a dr -> FPGroup a $ requires dr p
    FPUnit  dr   -> FPUnit    $ requires dr p
  breaks   =  flip $ \p -> \case
    FPGroup a dr -> FPGroup a $ breaks   dr p
    FPUnit  dr   -> FPUnit    $ breaks   dr p

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance Eq      (Named r a)   where (==) = (==) `on` _nName
instance Ord     (Named r a)   where compare = compare `on` _nName
--instance Show    (Named r a)   where show = show . _nName
instance Functor (Named r)     where fmap = (nMark %~)

instance Eq      (DepRule r a) where (==) = (==) `on` _drRule
instance Ord     (DepRule r a) where compare = compare `on` _drRule
--instance Show    (DepRule r a) where show = show . _drRule
instance Functor (DepRule r)   where fmap = (drRule %~) . fmap

instance Eq      (FPGroup m t a) where
  FPUnit    l == FPUnit    r = l == r
  FPGroup _ l == FPGroup _ r = l == r
instance Ord     (FPGroup m t a) where
  compare = curry $ \case
    (FPUnit    l, FPUnit    r) -> compare l r
    (FPGroup _ l, FPGroup _ r) -> compare l r
{-
instance Show    (FPGroup m t a) where
  show = \case FPUnit    l -> show l
               FPGroup _ l -> show l
-}
instance Functor (FPGroup m t)   where
  fmap f = \case
    FPGroup a r -> FPGroup a $ drRule %~ fmap f . (nRule %~ (f <$$>)) $ r
    FPUnit  u   -> FPUnit    $ fmap f u

--------------------------------------------------------------------------------
-- Config of rules:
--------------------------------------------------------------------------------

data RulePart = Wildcard | RPKey String
  deriving (Eq, Ord, Show, Read)
type RNConfig = [RulePart]
type KeepRule = Bool
data RuleConfig  = RuleConfig
  { _crnRule :: RNConfig
  , _crnKeep :: KeepRule }
  deriving (Eq, Ord, Show, Read)
type RulesConfig = [RuleConfig]

rpApplies :: (String, RulePart) -> Bool
rpApplies = \case
  (_ , Wildcard) -> True
  (pr, RPKey pc) -> pr == pc

cfgApplies :: RuleName -> RNConfig -> Bool
cfgApplies rn rnc =
  let (pairs, (remRN, remRNC)) = zipRem (_rname rn) rnc
      pairsApp = all rpApplies pairs
  in if   null remRN then if not (null remRNC) then False else pairsApp
     else maybe False ((Wildcard ==) . snd . fst) (uncons $ reverse pairs) &&
          pairsApp

testRule :: RuleName -> RulesConfig -> KeepRule
testRule rn = foldl' f True
  where f = (\acc (RuleConfig rc kr) -> if cfgApplies rn rc then kr else acc)

setMarkImm :: (Named r a -> KeepRule) -> DepRule r a -> DepRule r KeepRule
setMarkImm f = drRule %~ (\n -> nMark .~ (f n) $ n)

markImm :: RulesConfig -> DepRule r a -> DepRule r KeepRule
markImm rsc = setMarkImm $ \nr -> testRule (_nName nr) rsc

traverseSub :: Applicative f => LensLike f (DepRule [a] a1) (DepRule [b] a1) a b
traverseSub = drRule.nRule.traverse

-- | Mark whether rules are to be kept or not.
markRules :: RulesConfig -> FPGroup m t a -> FPGroup m t KeepRule
markRules rsc = \case
  FPUnit  u   -> FPUnit    $ markImm rsc u
  FPGroup a r -> FPGroup a $ markImm rsc . (traverseSub %~ markRules rsc) $ r

--------------------------------------------------------------------------------
-- Determination of available rules:
--------------------------------------------------------------------------------

type RuleMap m t = Map RuleName (FPGroup m t ())

addAllowed :: DepRule r KeepRule -> FPGroup m t a -> Writer (RuleMap m t) ()
addAllowed dr r = let nr = _drRule dr in
                  if _nMark nr then tell $ M.singleton (_nName nr) $ void r
                               else pure ()

-- | Computes a Map of {rule names -> rule} for available rules.
availRules :: FPGroup m t KeepRule -> RuleMap m t
availRules = execWriter . avail
  where avail r = case r of
         FPUnit  d   -> addAllowed d r
         FPGroup a d -> addAllowed d r >> void (traverseSub %%~ avail $ d)

--------------------------------------------------------------------------------
-- Determination of transitive removal:
--------------------------------------------------------------------------------

getMark :: DepRule r c -> c
getMark = _nMark . _drRule

markEmpty :: FPGroup m t KeepRule -> (Any, FPGroup m t KeepRule)
markEmpty r = case r of
  FPUnit v    -> (Any $ getMark v, r)
  FPGroup a r -> let mk = getMark r in second (FPGroup a) $
                 if mk then traverseSub %%~ markEmpty $ r else (Any mk, r)

checkReq :: RuleMap m t -> [RuleName] -> Bool
checkReq rmap reqs = getAll $ mconcat $ All . flip M.member rmap <$> reqs

markUnavailImm :: RuleMap m t -> DepRule r KeepRule -> DepRule r KeepRule
markUnavailImm rmap d = let reqs = _drPre d
                            mk   = getMark d
                        in  setMarkImm (const $ mk && checkReq rmap reqs) d

markUnavail :: RuleMap m t -> FPGroup m t KeepRule -> FPGroup m t KeepRule
markUnavail rmap r = case r of
  FPUnit    d -> FPUnit    $ markUnavailImm rmap d
  FPGroup a d -> FPGroup a $ markUnavailImm rmap .
                             (traverseSub %~ markUnavail rmap) $ d

markTrans :: FPGroup m t KeepRule -> (FPGroup m t KeepRule, RuleMap m t)
markTrans r0 = let r1 = snd $ markEmpty r0
                   rm = availRules r1
               in  (markUnavail rm r1, rm)

markFix :: FPGroup m t KeepRule -> (FPGroup m t KeepRule, RuleMap m t)
markFix r0 = let (r1, rm) = markTrans r0
             in  if r0 == r1 then (r0, rm) else markFix r1

unmarkFilter :: FPGroup m t KeepRule -> Maybe (FPGroup m t ())
unmarkFilter r = case r of
  FPUnit    d -> if nm d then Nothing else Just $ void r
  FPGroup a d -> if nm d then Nothing
                 else Just $ FPGroup a $ void $
                      drRule.nRule %~ catMaybes . fmap unmarkFilter $ d
  where nm = not . getMark

--------------------------------------------------------------------------------
-- Into ordering:
--------------------------------------------------------------------------------

ra, rb, rc, rd :: FPGroup Identity Int ()
ra = rule ["a"] unique
rb = rule ["b"] unique `requires` [["a"]]
rc = rule ["c"] unique `requires` [["b"], ["a"]] `breaks` [["a"]]
rd = rule ["d"] unique `requires` [["b"]] `breaks` [["b"]]
gr = fpGroup ["grp"] [rd, rc]

cfg = [RuleConfig [RPKey "a"] False]
mark = --unmarkFilter $ fst $
        markFix $ markRules cfg gr

{-



-}

--------------------------------------------------------------------------------
-- Logging of applied rules:
--------------------------------------------------------------------------------

data RuleLogEntry
  = RLEUnit   RuleName
  | RLEGStart RuleName
  | RLEGEnd
  deriving (Eq, Ord, Show, Read)

type RuleLog = DList RuleLogEntry

--------------------------------------------------------------------------------
-- Logged list to Tree of applied rules:
--------------------------------------------------------------------------------

type RLStack = NE.NonEmpty (DTree RuleName)
type RLTComp = StateT RLStack (Except ()) ()

rlTreeComp :: RuleLogEntry -> RLTComp
rlTreeComp = \case
  RLEUnit   n -> modify $ mhead $ dtSnocL n
  RLEGStart n -> modify $ cons (dtLeaf n)
  RLEGEnd     -> NE.uncons <$> get >>= \(h, m) ->
                 maybe (throwError ()) (put . (mhead $ dtSnoc h)) m

toMaybe :: Either e a -> Maybe a
toMaybe = either (const Nothing) pure

entryStack :: RLStack
entryStack = dtLeaf rootRule :| []

rlToTree :: [RuleLogEntry] -> Maybe (Tree RuleName)
rlToTree log = let comp  = mapM_ rlTreeComp log
                   state = runExcept $ execStateT comp entryStack
               in  toTree . NE.head <$> toMaybe state

{-
ra = RuleName ["a", "b", "c"]
rb = RuleName ["d", "e", "f"]
rc = RuleName ["g", "h", "i"]
rd = RuleName ["j", "k", "l"]
re = RuleName ["m", "n", "o"]
rf = RuleName ["p", "q", "r"]
rg = RuleName ["s", "t", "u"]
rh = RuleName ["v", "x", "y"]
rllog = [
      RLEUnit ra
    , RLEUnit rb
    , RLEGStart rc
      , RLEUnit rd
      , RLEGStart re
        , RLEUnit rf
        , RLEUnit rg
      , RLEGEnd
    , RLEGEnd
    , RLEUnit rh
    ]
-}

--------------------------------------------------------------------------------
-- Execution:
--------------------------------------------------------------------------------

type LogNormT m a = NormT (WriterT RuleLog m) a
type LogNArrT m a = a -> LogNormT m a

addLog :: RuleName -> (t, Unique) -> ((t, Unique), RuleLog)
addLog n res = let c = isChange $ snd res
               in  (res, if c then singleton $ RLEUnit n else empty)

logRule :: Monad m => NamedRule m t -> LogNArrT m t
logRule (Named _ n r) t = mkNorm $ WriterT $ addLog n <$> runNT (r t)