-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Propagator.Change
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the types and functions needed to describe changes of
-- cell values.
-----------------------------------------------------------------------------
module Data.Propagator.Change(Change(..)) where

-- | Represents a potential change of a cell value.
data Change a
  = Changed a
  -- ^ Indicates that a cell value has been changed to the new value @a@.
  | Unchanged a
  -- ^ Indicates that a cell value did not change, i.e. needs no propagation.
  | Incompatible
  -- ^ Indicates that a new cell value contradicts the one that is already
  -- stored in the cell.
  deriving (Eq, Functor, Ord, Show)

instance Applicative Change where
  pure = Unchanged
  mf <*> ma =
    case mf of
      Changed f ->
        case ma of
          Changed a    -> Changed (f a)
          Unchanged a  -> Changed (f a)
          Incompatible -> Incompatible
      Unchanged f ->
        fmap f ma
      Incompatible ->
        Incompatible

instance Monad Change where
  m >>= f =
    case m of
      Changed a ->
        case f a of
          Unchanged b -> Changed b
          other       -> other
      Unchanged a ->
        f a
      Incompatible ->
        Incompatible