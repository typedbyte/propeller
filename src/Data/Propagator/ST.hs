-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Propagator.ST
-- Copyright   :  (c) Michael Szvetits, 2024
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports ST-based types and functions needed to create cells,
-- manipulate their data and wire them up for data propagation.
-----------------------------------------------------------------------------
module Data.Propagator.ST
  ( -- * Networks
    Propagation
  , undo
  , succeeded
    -- ** Cells
  , Cell
  , Change(..)
  , cell
  , readCell
  , writeCell
  , label
    -- ** Connections
  , connect
  , sync
  , syncWith
  , propagate
  , propagateMany
    -- *** Numeric
  , plus
  , minus
  , times
  , timesWith
  , abs
  , absWith
  , negate
  , signum
  , signumWith
  ) where

-- base
import Control.Monad    (forM)
import Control.Monad.ST (ST)
import Data.STRef       (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)

import Prelude hiding (abs, negate, signum)
import Prelude qualified as Prelude

import Data.Propagator.Change (Change(..))

-- | The result of a propagation which allows to rollback changes and inspect
-- its success.
data Propagation s = Propagation
  { undo :: ST s ()
    -- ^ An action that reverts all cell changes of a propagation (both direct
    -- and transitive ones).
  , succeeded :: Bool
    -- ^ 'True' if the propagation was successful (i.e., it did not lead to a
    -- cell change that is 'Incompatible'), otherwise 'False'.
    --
    -- Note that unsuccessful propagations are not automatically reverted. Use
    -- 'undo' to do this.
  }

addUndo :: ST s () -> Propagation s -> Propagation s
addUndo action (Propagation us r) = Propagation (us >> action) r

type Propagator s a = a -> ST s (Propagation s)

-- | The type of a cell holding a value of type @a@. The type parameter @s@
-- serves to keep the internal states of different cell networks separate from
-- each other (see 'ST' for details).
data Cell s a = Cell
  { _join    :: a -> a -> Change a
  , valueRef :: STRef s a
  , propRef  :: STRef s (Propagator s a)
  }

instance Eq (Cell s a) where
  Cell _ lr _ == Cell _ rr _ = lr == rr

-- | Constructs a new cell with a given initial value and a function which
-- defines how to react if a new value is about to be written to the cell.
cell :: a                    -- ^ The initial value of the cell.
     -> (a -> a -> Change a) -- ^ A function that describes how to join
                             --   an existing cell value with a new one that
                             --   the cell has received via propagation.
     -> ST s (Cell s a)      -- ^ The newly constructed cell.
cell value f = do
  v <- newSTRef value
  n <- newSTRef emptyPropagator
  pure (Cell f v n)

-- | Reads the value of a specific cell.
readCell :: Cell s a -> ST s a
readCell = readSTRef . valueRef

-- | Writes a new value to a specific cell and starts to propagate potential
-- changes through the network of connected cells.
writeCell :: a -> Cell s a -> ST s (Propagation s)
writeCell new (Cell f vRef pRef) = do
  old <- readSTRef vRef
  case f old new of
    Unchanged _  -> pure success
    Incompatible -> pure failure
    Changed n    -> do
      writeSTRef vRef n
      propagator  <- readSTRef pRef
      propagation <- propagator n
      pure (addUndo (writeSTRef vRef old) propagation)

emptyPropagator :: Propagator s a
emptyPropagator = const (pure success)

failure :: Propagation s
failure = Propagation (pure ()) False

success :: Propagation s
success = Propagation (pure ()) True

attach :: Propagator s a -> STRef s (Propagator s a) -> ST s ()
attach newProp pRef =
  modifySTRef pRef $
    \currentProp a ->
      chain (currentProp a) (newProp a)

-- | Connects a source cell to a target cell in order to propagate changes
-- from the source to the target.
--
-- Note that newly connected cells do not start to propagate changes
-- immediately after wiring up. Use 'propagate' or 'propagateMany' to do this.
connect :: Cell s a      -- ^ The source cell.
        -> Cell s b      -- ^ The target cell.
        -> (a -> ST s b) -- ^ A function that describes how the value for the
                         --   target cell is constructed, based on the value of
                         --   the source cell.
        -> ST s ()       -- ^ Note that no propagation takes place (i.e., no
                         --   'Propagation' is returned).
connect source target f =
  flip attach (propRef source) $
    \a -> do
      newValue <- f a
      writeCell newValue target

-- | Connects and synchronizes two cells, i.e. new values are propagated from
-- the source to the target cell, and vice versa. Short form of 'syncWith'
-- 'id' 'id'.
--
-- Note that newly connected cells do not start to propagate changes
-- immediately after wiring up. Use 'propagate' or 'propagateMany' to do this.
sync :: Cell s a -> Cell s a -> ST s ()
sync = syncWith id id

-- | Connects and synchronizes two cells using two translation functions @f@
-- and @g@, i.e. new values are propagated from the source to the target cell
-- using @f@, and vice versa using @g@.
--
-- Note that newly connected cells do not start to propagate changes
-- immediately after wiring up. Use 'propagate' or 'propagateMany' to do this.
syncWith :: (a -> b) -> (b -> a) -> Cell s a -> Cell s b -> ST s ()
syncWith f g left right = do
  connect left right (pure . f)
  connect right left (pure . g)

-- | Propagates the value of a specific cell to its connected cells in a
-- transitive manner. The propagation ends if no more cell changes occur or if
-- an 'Incompatible' cell value change is encountered.
propagate :: Cell s a -> ST s (Propagation s)
propagate source = do
  value      <- readSTRef (valueRef source)
  propagator <- readSTRef (propRef source)
  propagator value

-- | Propagates the values of specific cells to their connected cells in a
-- transitive manner. The propagation ends if no more cell changes occur or if
-- an 'Incompatible' cell value change is encountered.
propagateMany :: [Cell s a] -> ST s (Propagation s)
propagateMany []     = pure success
propagateMany (c:cs) = chain (propagate c) (propagateMany cs)

chain :: ST s (Propagation s) -> ST s (Propagation s) -> ST s (Propagation s)
chain prop continue = do
  propagation <- prop
  if succeeded propagation then do
    rest <- continue
    pure (addUndo (undo propagation) rest)
  else
    pure propagation

-- | If the content of a @Cell s a@ is an accumulation of multiple values @[b]@,
-- and every value @b@ itself can be used as content @a@ for the cell, then we
-- can write every value @b@ one after another to the cell and check if the
-- network converges to a successful state.
--
-- As a result, we can enumerate all possible combinations of valid values for
-- a given set of cells. This is often used in constraint solving algorithms.
label :: (a -> [b]) -- ^ A function which extracts testable values from a cell
                    --   content.
      -> (b -> a)   -- ^ A function which translates a testable value into a
                    --   value which can be written back to the cell.
      -> [Cell s a] -- ^ The set of cells for which the values are enumerated.
      -> ST s [[b]] -- ^ Returns all valid assignments for the given cells.
label elems reify cells = solve [] (reverse cells)
  where
    solve current []     = pure [current]
    solve current (c:cs) = do
      cellValue <- readCell c
      solutions <-
        forM (elems cellValue) $ \v -> do
          propagation <- writeCell (reify v) c
          vSolutions <-
            if succeeded propagation then
              solve (v:current) cs
            else
              pure []
          undo propagation
          pure vSolutions
      pure (concat solutions)

-- | @plus a b c@ connects three cells using the following propagation schema:
--
-- * @a + b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @c - b@ is propagated to @a@ if @b@ or @c@ changes.
-- * @c - a@ is propagated to @b@ if @a@ or @c@ changes.
plus :: Num a => Cell s a -> Cell s a -> Cell s a -> ST s ()
plus left right result = do
  connect left result  (\lv -> (lv +)      <$> readCell right)
  connect left right   (\lv -> subtract lv <$> readCell result)
  connect right result (\rv -> (+ rv)      <$> readCell left)
  connect right left   (\rv -> subtract rv <$> readCell result)
  connect result left  (\sv -> (sv -)      <$> readCell right)
  connect result right (\sv -> (sv -)      <$> readCell left)

-- | @minus a b c@ connects three cells using the following propagation schema:
--
-- * @a - b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @b + c@ is propagated to @a@ if @b@ or @c@ changes.
-- * @a - c@ is propagated to @b@ if @a@ or @c@ changes.
minus :: Num a => Cell s a -> Cell s a -> Cell s a -> ST s ()
minus left right result = do
  connect left result  (\lv -> (lv -)      <$> readCell right)
  connect left right   (\lv -> (lv -)      <$> readCell result)
  connect right result (\rv -> subtract rv <$> readCell left)
  connect right left   (\rv -> (+ rv)      <$> readCell result)
  connect result left  (\dv -> (+ dv)      <$> readCell right)
  connect result right (\dv -> subtract dv <$> readCell left)

-- | @times a b c@ connects three cells using the following propagation schema:
--
-- * @a * b@ is propagated to @c@ if @a@ or @b@ changes.
times :: Num a => Cell s a -> Cell s a -> Cell s a -> ST s ()
times left right result = do
  connect left result  (\lv -> (lv *) <$> readCell right)
  connect right result (\rv -> (* rv) <$> readCell left)

-- | @timesWith divOp a b c@ connects three cells using the following propagation schema:
--
-- * @a * b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @divOp c b@ is propagated to @a@ if @b@ or @c@ changes.
-- * @divOp c a@ is propagated to @b@ if @a@ or @c@ changes.
timesWith :: Num a => (a -> a -> a) -> Cell s a -> Cell s a -> Cell s a -> ST s ()
timesWith divOp left right result = do
  times left right result
  connect left right   (\lv -> (`divOp` lv) <$> readCell result)
  connect right left   (\rv -> (`divOp` rv) <$> readCell result)
  connect result left  (\pv -> (pv `divOp`) <$> readCell right)
  connect result right (\pv -> (pv `divOp`) <$> readCell left)

-- | @abs a b@ connects two cells using the following propagation schema:
--
-- * @|a|@ is propagated to @b@ if @a@ changes.
abs :: Num a => Cell s a -> Cell s a -> ST s ()
abs c result =
  connect c result (pure . Prelude.abs)

-- | @absWith inv a b@ connects two cells using the following propagation schema:
--
-- * @|a|@ is propagated to @b@ if @a@ changes.
-- * @inv b@ is propagated to @a@ if @b@ changes.
absWith :: Num a => (a -> a) -> Cell s a -> Cell s a -> ST s ()
absWith inv c result = do
  connect c result (pure . Prelude.abs)
  connect result c (pure . inv)

-- | @negate a b@ connects two cells using the following propagation schema:
--
-- * @-a@ is propagated to @b@ if @a@ changes.
-- * @-b@ is propagated to @a@ if @b@ changes.
negate :: Num a => Cell s a -> Cell s a -> ST s ()
negate c result = do
  connect c result (pure . Prelude.negate)
  connect result c (pure . Prelude.negate)

-- | @signum a b@ connects two cells using the following propagation schema:
--
-- * @Prelude.signum a@ is propagated to @b@ if @a@ changes.
signum :: Num a => Cell s a -> Cell s a -> ST s ()
signum c result =
  connect c result (pure . Prelude.signum)

-- | @signumWith inv a b@ connects two cells using the following propagation schema:
--
-- * @Prelude.signum a@ is propagated to @b@ if @a@ changes.
-- * @inv b@ is propagated to @a@ if @b@ changes.
signumWith :: Num a => (a -> a) -> Cell s a -> Cell s a -> ST s ()
signumWith inv c result = do
  connect c result (pure . Prelude.signum)
  connect result c (pure . inv)