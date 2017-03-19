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

{-# LANGUAGE LambdaCase #-}

-- | Asorted debugging utilities during development.
-- Should not be used in production.
module Util.Debug (
  -- ** Operations
    fromRight
  , exitLeft
  , pretty
  , pPrint
  , hcPrint
  ) where

import System.Exit (exitFailure)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Language.Haskell.HsColour.Colourise
import Language.Haskell.HsColour.ColourHighlight
import Language.Haskell.HsColour (ColourPrefs (..))
import Language.Haskell.HsColour.TTY
import qualified Text.Pretty.Simple as P
import Data.Text.Lazy (unpack)

-- | Partial function for extracting the Right value of an Either.
fromRight :: Either e a -> a
fromRight = either undefined id

-- | Given an Either with a showable error, it exits on Left, and otherwise
-- yields the pure value.
exitLeft :: Show a => Either a b -> IO b
exitLeft = \case
  Left  e -> print e >> exitFailure
  Right a -> pure a

--------------------------------------------------------------------------------
-- Pretty printing:
--------------------------------------------------------------------------------

pretty :: Show a => a -> String
pretty = unpack . P.pShowOpt (P.OutputOptions 2 Nothing)

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = P.pPrintOpt $ P.OutputOptions 2 $ Just P.defaultColorOptionsDarkBg

cp :: ColourPrefs
cp = ColourPrefs
  { keyword  = [Foreground Green,Underscore]
  , keyglyph = [Foreground Red]
  , layout   = [Foreground Cyan]
  , comment  = [Foreground Blue, Italic]
  , conid    = [Bold, Foreground White]
  , varid    = [Normal]
  , conop    = [Foreground Red,Bold]
  , varop    = [Foreground Cyan]
  , string   = [Foreground Green, Bold]
  , char     = [Foreground Yellow, Bold]
  , number   = [Foreground Green, Bold]
  , cpp      = [Foreground Magenta,Dim]
  , selection = [Bold, Foreground Yellow]
  , variantselection = [Dim, Foreground Red, Underscore]
  , definition = [Foreground Blue]
  }

hcPrint :: (MonadIO m, Show a) => a -> m ()
hcPrint = liftIO . putStrLn . hscolour cp . pretty