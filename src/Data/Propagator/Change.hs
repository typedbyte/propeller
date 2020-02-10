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

-- | A type which describes a potential change of a cell value.
data Change a
  -- | Indicates that a cell value has been changed to the new value @a@.
  = Changed a
  -- | Indicates that a cell value did not change, i.e. needs no propagation.
  | Unchanged
  -- | Indicates that a new cell value contradicts the one that is already
  -- stored in the cell.
  | Incompatible
  deriving (Eq, Show)