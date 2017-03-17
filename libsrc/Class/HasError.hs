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

{-# LANGUAGE KindSignatures, MultiParamTypeClasses, FunctionalDependencies
  , FlexibleInstances #-}

-- | A class of things with potential error, which is extractable, and some
-- instances for common types.
module Class.HasError (
  -- * Class
    HasError
  ) where

import Control.Comonad (Comonad, extract)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

--------------------------------------------------------------------------------
-- Class:
--------------------------------------------------------------------------------

-- | A class of things with potential error, which is extractable
class HasError (e :: *) (m :: * -> *) | m -> e where
  -- | 'extractError' extracts the error from some structure.
  extractError :: m a -> Maybe e

--------------------------------------------------------------------------------
-- Instances:
--------------------------------------------------------------------------------

instance HasError () Maybe where
  extractError = maybe (pure ()) (const Nothing)

instance HasError e (Either e) where
  extractError = either pure (const Nothing)

instance Comonad m => HasError () (MaybeT m) where
  extractError = extractError . extract . runMaybeT

instance Comonad m => HasError e (ExceptT e m) where
  extractError = extractError . extract . runExceptT