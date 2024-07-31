-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Propagator
-- Copyright   :  (c) Michael Szvetits, 2024
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the types and functions needed to create cells,
-- manipulate their data and wire them up for data propagation.
-----------------------------------------------------------------------------
module Data.Propagator
  ( -- * Networks
    Network
  , empty
  , Error(..)
  , Propagator
  , runPropagator
    -- ** Cells
  , CellKey(..)
  , Change(..)
  , cell
  , readCell
  , writeCell
  , removeCell
  , label
    -- ** Connections
  , ConnectKey(..)
  , ConnectState(..)
  , connect
  , connect_
  , sync
  , sync_
  , syncWith
  , syncWith_
  , combine
  , combine_
  , combineMany
  , combineMany_
  , distribute
  , distribute_
  , manyToMany
  , manyToMany_
  , disconnect
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
import Control.Monad        (forM, void)
import Data.Foldable        (traverse_)
import Data.Functor.Compose (Compose(..))

import Prelude hiding (abs, negate, signum)
import Prelude qualified as Prelude

-- containers
import Data.IntMap.Strict qualified as M
import Data.IntSet        qualified as S

import Data.Propagator.Change (Change(..))

-- | Represents a network cell holding a value of type @a@.
data Cell a = Cell
  { value :: !a
  , update :: !(a -> a -> Change a)
  , subscribers :: !S.IntSet
  , incomings :: !S.IntSet
  }

-- | Represents a unique identification of a network cell.
--
-- Such an identification should not be constructed manually, or else
-- 'InvalidCell' errors are possible. The raw identification is exposed for
-- situations where cells should be managed externally, like in an 'M.IntMap'.
newtype CellKey = CellKey { rawCellKey :: Int }
  deriving (Eq, Ord, Show)

data Prop a = Prop
  { sources :: ![CellKey]
  , targets :: ![CellKey]
  , action :: !(ConnectKey -> Propagator a ())
  }

-- | Represents a unique identification of a network connection.
--
-- Such an identification should not be constructed manually, or else
-- 'InvalidConnect' errors are possible. The raw identification is exposed for
-- situations where connections should be managed externally, like in an 'M.IntMap'.
newtype ConnectKey = ConnectKey { rawConnectKey :: Int }
  deriving (Eq, Ord, Show)

-- | A network consists of cells and connections which propagate data between them.
data Network a = Network
  { nextCellKey :: !CellKey
  , nextConnectKey :: !ConnectKey
  , cells :: !(M.IntMap (Cell a))
  , propagators :: !(M.IntMap (Prop a))
  }

-- | Network modifications and data propagations are captured by the 'Propagator' monad.
newtype Propagator a b =
  Propagator
    { runPropagator :: Network a -> Either (Error a) (Network a, b)
      -- ^ Applies modifications captured by the propagator monad to a network,
      -- thus producing a new network if no error occurred.
    }
  deriving Functor

instance Applicative (Propagator a) where
  pure a =
    Propagator (\net -> Right (net, a))
  pf <*> pa =
    Propagator $ \net -> do
      (net',f) <- runPropagator pf net
      (net'',a) <- runPropagator pa net'
      pure (net'', f a)

instance Monad (Propagator a) where
  Propagator f >>= g =
    Propagator $ \net -> do
      (net',a) <- f net
      runPropagator (g a) net'

-- | When connecting cells, the 'ConnectState' defines the initial behaviour of the connection.
data ConnectState
  = Live -- ^ The connection immediately starts to propagate data between the connected cells.
  | Idle -- ^ The connection is established, but no initial data propagation takes place.
  deriving (Eq, Ord, Show)

failWith :: Error b -> Propagator b a
failWith e = Propagator $ \_ -> Left e

addPropagator :: Prop a -> Propagator a ConnectKey
addPropagator prop =
  Propagator $ \net ->
    let
      key@(ConnectKey nextInt) = nextConnectKey net
    in
      Right
        ( net
            { nextConnectKey = ConnectKey (nextInt + 1)
            , propagators = M.insert nextInt prop (propagators net)
            }
        , key
        )

getCell :: CellKey -> Propagator a (Cell a)
getCell ck@(CellKey k) =
  Propagator $ \net ->
    toError (InvalidCell ck) $ do
      prop <- M.lookup k (cells net)
      pure (net, prop)

getPropagator :: ConnectKey -> Propagator a (Prop a)
getPropagator key@(ConnectKey k) =
  Propagator $ \net ->
    toError (InvalidConnect key) $ do
      prop <- M.lookup k (propagators net)
      pure (net, prop)

extractPropagator :: ConnectKey -> Propagator a (Prop a)
extractPropagator key@(ConnectKey k) =
  Propagator $ \net ->
    let
      (maybeProp, newPropagators) =
        M.updateLookupWithKey (\_ _ -> Nothing) k (propagators net)
    in do
      prop <- toError (InvalidConnect key) maybeProp
      pure (net { propagators = newPropagators }, prop)

modifyCells :: (M.IntMap (Cell a) -> M.IntMap (Cell a)) -> Propagator a ()
modifyCells f =
  Propagator $ \net ->
    Right
      (net { cells = f (cells net) }, ())

modifyCell :: (Cell a -> Cell a) -> CellKey -> Propagator a ()
modifyCell f key@(CellKey k) =
  Propagator $ \net -> do
    newCells <- M.alterF g k (cells net)
    pure (net { cells = newCells }, ())
  where
    g Nothing  = Left (InvalidCell key)
    g (Just c) = Right $ Just (f c)

-- | Represents an empty network.
empty :: Network a
empty = Network (CellKey 0) (ConnectKey 0) M.empty M.empty

-- | Constructs a new cell with a given initial value and a function which
-- defines how to react if a new value is about to be written to the cell.
cell
  :: a
  -- ^ The initial value of the cell.
  -> (a -> a -> Change a)
  -- ^ A function that describes how to join an existing cell value with a
  -- new one that the cell has received via propagation.
  -> Propagator a CellKey
  -- ^ The identification of the newly constructed cell.
cell initValue f =
  Propagator $ \net ->
    let
      nextKey = nextCellKey net
      nextInt = rawCellKey nextKey
      newCell = Cell initValue f S.empty S.empty
      net' =
        net
          { nextCellKey = CellKey (nextInt + 1)
          , cells = M.insert nextInt newCell (cells net)
          }
    in
      Right (net', nextKey)

-- | Reads the value of a specific cell.
readCell :: CellKey -> Propagator a a
readCell k = value <$> getCell k

-- | Writes a new value to a specific cell and starts to propagate potential
-- changes through the network of connected cells.
writeCell :: a -> CellKey -> Propagator a ()
writeCell newValue ck@(CellKey k) =
  Propagator $ \net -> do
    (subs, newCells) <- getCompose $ M.alterF change k (cells net)
    flip runPropagator net { cells = newCells } $
      traverse_
        (fire . ConnectKey)
        (S.elems subs)
  where
    change maybeCell =
      Compose $
        case maybeCell of
          Nothing -> Left (InvalidCell ck)
          Just c@(Cell {value}) ->
            case update c value newValue of
              Changed new ->
                Right (subscribers c, Just c { value = new })
              Unchanged _ -> 
                Right (S.empty, Just c)
              Incompatible ->
                Left (Conflict ck value newValue)

-- | Removes a cell from the network. This also removes all connections related to the cell.
removeCell :: CellKey -> Propagator a ()
removeCell key@(CellKey k) = do
  theCell <- getCell key
  traverse_
    (disconnect . ConnectKey)
    (S.elems (subscribers theCell) ++ S.elems (incomings theCell))
  modifyCells (M.delete k)

-- | If the content of a cell is an accumulation of multiple values @[b]@,
-- and every value @b@ itself can be used as content @a@ for the cell, then we
-- can write every value @b@ one after another to the cell and check if the
-- network converges to a successful state.
--
-- As a result, we can enumerate all possible combinations of valid values for
-- a given set of cells. This is often used in constraint solving algorithms.
--
-- This function does not perform any permanent network modifications.
label :: (a -> [b]) -> (b -> a) -> [CellKey] -> Propagator a [[b]]
label elems reify keys = solve [] (reverse keys)
  where
    solve current []     = pure [current]
    solve current (k:ks) =
      Propagator $ \net -> do
        (net',a) <- runPropagator (readCell k) net
        solutions <-
          forM (elems a) $ \b ->
            case runPropagator (writeCell (reify b) k) net' of
              Right (net'',()) ->
                snd <$> runPropagator (solve (b:current) ks) net''
              Left _ ->
                pure []
        pure (net, concat solutions)

propagator :: ConnectState -> Prop a -> Propagator a ConnectKey
propagator state prop = do
  key@(ConnectKey k) <- addPropagator prop
  traverse_ (modifyCell $ addSub k) (sources prop)
  traverse_ (modifyCell $ addInc k) (targets prop)
  case state of
    Live -> fire key >> pure key
    Idle -> pure key
  where
    addSub k c = c { subscribers = S.insert k (subscribers c) }
    addInc k c = c { incomings = S.insert k (incomings c) }

-- | Connects a source cell to a target cell in order to propagate changes
-- from the source to the target. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
connect :: ConnectState -> CellKey -> CellKey -> (a -> Maybe a) -> Propagator a ConnectKey
connect state source target f =
  propagator state $ Prop [source] [target] $
    \key -> do
      ins <- readCell source
      out <- toPropagator (NoPropagation key) (f ins)
      writeCell out target

-- | Same as 'connect', but discards the returned 'ConnectKey'.
connect_ :: ConnectState -> CellKey -> CellKey -> (a -> Maybe a) -> Propagator a ()
connect_ state source target f =
  void $ connect state source target f

-- | Connects and synchronizes two cells, i.e. new values are propagated from
-- the source to the target cell, and vice versa. Short form of 'syncWith'
-- 'Just' 'Just'.
sync :: ConnectState -> CellKey -> CellKey -> Propagator a (ConnectKey, ConnectKey)
sync = syncWith Just Just

-- | Same as 'sync', but discards the returned 'ConnectKey's.
sync_ :: ConnectState -> CellKey -> CellKey -> Propagator a ()
sync_ state c1 c2 =
  void $ sync state c1 c2

-- | Connects and synchronizes two cells using two translation functions @f@
-- and @g@, i.e. new values are propagated from the source to the target cell
-- using @f@, and vice versa using @g@.
syncWith
  :: (a -> Maybe a)
  -> (a -> Maybe a)
  -> ConnectState
  -> CellKey
  -> CellKey
  -> Propagator a (ConnectKey, ConnectKey)
syncWith f g state c1 c2 = do
  key1 <- connect state c1 c2 f
  key2 <- connect state c2 c1 g
  pure (key1, key2)

-- | Same as 'syncWith', but discards the returned 'ConnectKey's.
syncWith_
  :: (a -> Maybe a)
  -> (a -> Maybe a)
  -> ConnectState
  -> CellKey
  -> CellKey
  -> Propagator a ()
syncWith_ f g state c1 c2 =
  void $ syncWith f g state c1 c2

-- | Connects two source cells to a target cell in order to propagate changes
-- from the sources to the target. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
combine :: ConnectState -> CellKey -> CellKey -> CellKey -> (a -> a -> Maybe a) -> Propagator a ConnectKey
combine state source1 source2 target f =
  propagator state $ Prop [source1,source2] [target] $
    \key -> do
      in1 <- readCell source1
      in2 <- readCell source2
      out <- toPropagator (NoPropagation key) (f in1 in2)
      writeCell out target

-- | Same as 'combine', but discards the returned 'ConnectKey'.
combine_ :: ConnectState -> CellKey -> CellKey -> CellKey -> (a -> a -> Maybe a) -> Propagator a ()
combine_ state source1 source2 target f =
  void $ combine state source1 source2 target f

-- | Connects several source cells to a target cell in order to propagate changes
-- from the sources to the target. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
combineMany :: ConnectState -> [CellKey] -> CellKey -> ([a] -> Maybe a) -> Propagator a ConnectKey
combineMany state sources target f =
  propagator state $ Prop sources [target] $
    \key -> do
      ins <- traverse readCell sources
      out <- toPropagator (NoPropagation key) (f ins)
      writeCell out target

-- | Same as 'combineMany', but discards the returned 'ConnectKey'.
combineMany_ :: ConnectState -> [CellKey] -> CellKey -> ([a] -> Maybe a) -> Propagator a ()
combineMany_ state sources target f =
  void $ combineMany state sources target f

-- | Connects a source cells to several target cells in order to propagate changes
-- from the source to the targets. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
distribute :: ConnectState -> CellKey -> [CellKey] -> (a -> Maybe a) -> Propagator a ConnectKey
distribute state source targets f =
  propagator state $ Prop [source] targets $
    \key -> do
      ins <- readCell source
      out <- toPropagator (NoPropagation key) (f ins)
      traverse_ (writeCell out) targets

-- | Same as 'distribute', but discards the returned 'ConnectKey'.
distribute_ :: ConnectState -> CellKey -> [CellKey] -> (a -> Maybe a) -> Propagator a ()
distribute_ state source targets f =
  void $ distribute state source targets f

-- | Connects several source cells to several target cells in order to propagate changes
-- from the sources to the targets. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
manyToMany :: ConnectState -> [CellKey] -> [CellKey] -> ([a] -> Maybe a) -> Propagator a ConnectKey
manyToMany state sources targets f =
  propagator state $ Prop sources targets $
    \key -> do
      ins <- traverse readCell sources
      out <- toPropagator (NoPropagation key) (f ins)
      traverse_ (writeCell out) targets

-- | Same as 'manyToMany', but discards the returned 'ConnectKey'.
manyToMany_ :: ConnectState -> [CellKey] -> [CellKey] -> ([a] -> Maybe a) -> Propagator a ()
manyToMany_ state sources targets f =
  void $ manyToMany state sources targets f

-- | Removes a connection from the network.
disconnect :: ConnectKey -> Propagator a ()
disconnect key@(ConnectKey k) = do
  prop <- extractPropagator key
  traverse_ (modifyCell removeSub) (sources prop)
  traverse_ (modifyCell removeInc) (targets prop)
    where
      removeSub c =
        c { subscribers = S.delete k (subscribers c) }
      removeInc c =
        c { incomings = S.delete k (incomings c) }

fire :: ConnectKey -> Propagator a ()
fire k = do
  prop <- getPropagator k
  action prop k

-- | Represents possible errors that may occur when modifying or using a network.
data Error a
  = InvalidCell CellKey
  -- ^ The specified cell could not be found.
  | InvalidConnect ConnectKey
  -- ^ The specified connection could not be found.
  | NoPropagation ConnectKey
  -- ^ The specified connection did not produce a value.
  | Conflict CellKey a a
  -- ^ The old value of the specified cell is incompatible with a new value propagated to it.
  deriving (Eq, Ord, Show)

toError :: Error b -> Maybe a -> Either (Error b) a
toError _ (Just a) = Right a
toError e Nothing  = Left e

toPropagator :: Error b -> Maybe a -> Propagator b a
toPropagator e m =
  case m of
    Just a  -> pure a
    Nothing -> failWith e

-- | @plus s a b c@ connects three cells using the following propagation schema:
--
-- * @a + b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @c - b@ is propagated to @a@ if @b@ or @c@ changes.
-- * @c - a@ is propagated to @b@ if @a@ or @c@ changes.
plus :: Num a => ConnectState -> CellKey -> CellKey -> CellKey -> Propagator a ()
plus state left right result = do
  combine_ state left right result (\lv rv -> Just (lv + rv))
  combine_ state left result right (\lv r  -> Just (r - lv))
  combine_ state right result left (\rv r  -> Just (r - rv))

-- | @minus s a b c@ connects three cells using the following propagation schema:
--
-- * @a - b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @b + c@ is propagated to @a@ if @b@ or @c@ changes.
-- * @a - c@ is propagated to @b@ if @a@ or @c@ changes.
minus :: Num a => ConnectState -> CellKey -> CellKey -> CellKey -> Propagator a ()
minus state left right result = do
  combine_ state left right result (\lv rv -> Just (lv - rv))
  combine_ state left result right (\lv r  -> Just (lv - r))
  combine_ state right result left (\rv r  -> Just (r + rv))

-- | @times s a b c@ connects three cells using the following propagation schema:
--
-- * @a * b@ is propagated to @c@ if @a@ or @b@ changes.
times :: Num a => ConnectState -> CellKey -> CellKey -> CellKey -> Propagator a ()
times state left right result =
  combine_ state left right result (\lv rv -> Just (lv * rv))

-- | @timesWith divOp s a b c@ connects three cells using the following propagation schema:
--
-- * @a * b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @divOp c b@ is propagated to @a@ if @b@ or @c@ changes.
-- * @divOp c a@ is propagated to @b@ if @a@ or @c@ changes.
timesWith :: Num a => (a -> a -> a) -> ConnectState -> CellKey -> CellKey -> CellKey -> Propagator a ()
timesWith divOp state left right result = do
  times state left right result
  combine_ state left result right (\lv r -> Just (divOp r lv))
  combine_ state right result left (\rv r -> Just (divOp r rv))

-- | @abs s a b@ connects two cells using the following propagation schema:
--
-- * @|a|@ is propagated to @b@ if @a@ changes.
abs :: Num a => ConnectState -> CellKey -> CellKey -> Propagator a ()
abs state left right =
  connect_ state left right (Just . Prelude.abs)

-- | @absWith inv s a b@ connects two cells using the following propagation schema:
--
-- * @|a|@ is propagated to @b@ if @a@ changes.
-- * @inv b@ is propagated to @a@ if @b@ changes.
absWith :: Num a => (a -> a) -> ConnectState -> CellKey -> CellKey -> Propagator a ()
absWith inv state left right = do
  abs state left right
  connect_ state right left (Just . inv)

-- | @negate s a b@ connects two cells using the following propagation schema:
--
-- * @-a@ is propagated to @b@ if @a@ changes.
-- * @-b@ is propagated to @a@ if @b@ changes.
negate :: Num a => ConnectState -> CellKey -> CellKey -> Propagator a ()
negate state left right = do
  connect_ state left right (Just . Prelude.negate)
  connect_ state right left (Just . Prelude.negate)

-- | @signum s a b@ connects two cells using the following propagation schema:
--
-- * @Prelude.signum a@ is propagated to @b@ if @a@ changes.
signum :: Num a => ConnectState -> CellKey -> CellKey -> Propagator a ()
signum state left right =
  connect_ state left right (Just . Prelude.signum)

-- | @signumWith inv s a b@ connects two cells using the following propagation schema:
--
-- * @Prelude.signum a@ is propagated to @b@ if @a@ changes.
-- * @inv b@ is propagated to @a@ if @b@ changes.
signumWith :: Num a => (a -> a) -> ConnectState -> CellKey -> CellKey -> Propagator a ()
signumWith inv state left right = do
  signum state left right
  connect_ state right left (Just . inv)