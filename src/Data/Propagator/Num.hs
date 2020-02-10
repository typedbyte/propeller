-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Propagator.Num
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides convenience functions to connect cells which hold
-- numeric values.
-----------------------------------------------------------------------------
module Data.Propagator.Num where

-- base
import Control.Monad.ST (ST)

import Data.Propagator.Cell (Cell, connect, readCell)

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
timesWith inverseTimes left right result = do
  times left right result
  connect left right   (\lv -> (`inverseTimes` lv) <$> readCell result)
  connect right left   (\rv -> (`inverseTimes` rv) <$> readCell result)
  connect result left  (\pv -> (pv `inverseTimes`) <$> readCell right)
  connect result right (\pv -> (pv `inverseTimes`) <$> readCell left)

-- | @abs a b@ connects two cells using the following propagation schema:
--
-- * @|a|@ is propagated to @b@ if @a@ changes.
abs :: Num a => Cell s a -> Cell s a -> ST s ()
abs cell result =
  connect cell result (pure . Prelude.abs)

-- | @absWith inv a b@ connects two cells using the following propagation schema:
--
-- * @|a|@ is propagated to @b@ if @a@ changes.
-- * @inv b@ is propagated to @a@ if @b@ changes.
absWith :: Num a => (a -> a) -> Cell s a -> Cell s a -> ST s ()
absWith inverseAbs cell result = do
  connect cell result (pure . Prelude.abs)
  connect result cell (pure . inverseAbs)

-- | @negate a b@ connects two cells using the following propagation schema:
--
-- * @-a@ is propagated to @b@ if @a@ changes.
-- * @-b@ is propagated to @a@ if @b@ changes.
negate :: Num a => Cell s a -> Cell s a -> ST s ()
negate cell result = do
  connect cell result (pure . Prelude.negate)
  connect result cell (pure . Prelude.negate)

-- | @signum a b@ connects two cells using the following propagation schema:
--
-- * @Prelude.signum a@ is propagated to @b@ if @a@ changes.
signum :: Num a => Cell s a -> Cell s a -> ST s ()
signum cell result =
  connect cell result (pure . Prelude.signum)

-- | @signumWith inv a b@ connects two cells using the following propagation schema:
--
-- * @Prelude.signum a@ is propagated to @b@ if @a@ changes.
-- * @inv b@ is propagated to @a@ if @b@ changes.
signumWith :: Num a => (a -> a) -> Cell s a -> Cell s a -> ST s ()
signumWith inverseSignum cell result = do
  connect cell result (pure . Prelude.signum)
  connect result cell (pure . inverseSignum)