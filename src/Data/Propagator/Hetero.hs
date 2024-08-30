{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Propagator.Hetero
-- Copyright   :  (c) Michael Szvetits, 2024
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the types and functions needed to create cells of
-- heterogeneous types, manipulate their data and wire them up for data
-- propagation.
-----------------------------------------------------------------------------
module Data.Propagator.Hetero
  ( -- * Networks
    Network
  , empty
  , Error(..)
  , Propagator
  , runPropagator
    -- ** Cells
  , CellKey(..)
  , CellKeys(..)
  , CellValues(..)
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
import Data.Kind            (Type)
import Unsafe.Coerce        (unsafeCoerce)

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
newtype CellKey (a :: Type) = CellKey { rawCellKey :: Int }
  deriving (Eq, Ord, Show)

-- | Represents a list of cell identifications of heterogeneous types.
data CellKeys ts where
  KNil  :: CellKeys '[]
  KCons :: CellKey a -> CellKeys ts -> CellKeys (a ': ts)

infixr 3 `KCons`

-- | Represents a list of cell values of heterogeneous types.
data CellValues ts where
  VNil  :: CellValues '[]
  VCons :: a -> CellValues ts -> CellValues (a ': ts)

infixr 3 `VCons`

data Some t = forall a. Some (t a)

data Prop = Prop
  { sources :: ![Some CellKey]
  , targets :: ![Some CellKey]
  , action  :: !(ConnectKey -> Propagator ())
  }

-- | Represents a unique identification of a network connection.
--
-- Such an identification should not be constructed manually, or else
-- 'InvalidConnect' errors are possible. The raw identification is exposed for
-- situations where connections should be managed externally, like in an 'M.IntMap'.
newtype ConnectKey = ConnectKey { rawConnectKey :: Int }
  deriving (Eq, Ord, Show)

-- | A network consists of cells and connections which propagate data between them.
data Network = Network
  { nextRawCellKey :: !Int
  , nextRawConnectKey :: !Int
  , cells :: !(M.IntMap (Some Cell))
  , propagators :: !(M.IntMap Prop)
  }

-- | Network modifications and data propagations are captured by the 'Propagator' monad.
newtype Propagator a =
  Propagator
    { runPropagator :: Network -> Either Error (Network, a)
      -- ^ Applies modifications captured by the propagator monad to a network,
      -- thus producing a new network if no error occurred.
    }
  deriving Functor

instance Applicative Propagator where
  pure a =
    Propagator (\net -> Right (net, a))
  pf <*> pa =
    Propagator $ \net -> do
      (net',f) <- runPropagator pf net
      (net'',a) <- runPropagator pa net'
      pure (net'', f a)

instance Monad Propagator where
  Propagator f >>= g =
    Propagator $ \net -> do
      (net',a) <- f net
      runPropagator (g a) net'

-- | When connecting cells, the 'ConnectState' defines the initial behaviour of the connection.
data ConnectState
  = Live -- ^ The connection immediately starts to propagate data between the connected cells.
  | Idle -- ^ The connection is established, but no initial data propagation takes place.
  deriving (Eq, Ord, Show)

readValues :: CellKeys ts -> Propagator (CellValues ts)
readValues KNil = pure VNil
readValues (KCons key ts) = do
  value <- readCell key
  other <- readValues ts
  pure (value `VCons` other)

someKeys :: CellKeys ts -> [Some CellKey]
someKeys KNil = []
someKeys (KCons key ts) = Some key : someKeys ts

failWith :: Error -> Propagator a
failWith e = Propagator $ \_ -> Left e

addPropagator :: Prop -> Propagator ConnectKey
addPropagator prop =
  Propagator $ \net ->
    let
      nextInt = nextRawConnectKey net
    in
      Right
        ( net
            { nextRawConnectKey = nextInt + 1
            , propagators = M.insert nextInt prop (propagators net)
            }
        , ConnectKey nextInt
        )

getCell :: CellKey a -> Propagator (Cell a)
getCell ck@(CellKey k) =
  Propagator $ \net ->
    toError (InvalidCell (Some ck)) $ do
      Some prop <- M.lookup k (cells net)
      pure (net, unsafeCoerce prop)

getPropagator :: ConnectKey -> Propagator Prop
getPropagator key@(ConnectKey k) =
  Propagator $ \net ->
    toError (InvalidConnect key) $ do
      prop <- M.lookup k (propagators net)
      pure (net, prop)

extractPropagator :: ConnectKey -> Propagator Prop
extractPropagator key@(ConnectKey k) =
  Propagator $ \net ->
    let
      (maybeProp, newPropagators) =
        M.updateLookupWithKey (\_ _ -> Nothing) k (propagators net)
    in do
      prop <- toError (InvalidConnect key) maybeProp
      pure (net { propagators = newPropagators }, prop)

modifyCells :: (M.IntMap (Some Cell) -> M.IntMap (Some Cell)) -> Propagator ()
modifyCells f =
  Propagator $ \net ->
    Right
      (net { cells = f (cells net) }, ())

modifyCell :: (Some Cell -> Some Cell) -> Some CellKey -> Propagator ()
modifyCell f (Some key@(CellKey k)) =
  Propagator $ \net -> do
    newCells <- M.alterF g k (cells net)
    pure (net { cells = newCells }, ())
  where
    g Nothing  = Left (InvalidCell (Some key))
    g (Just c) = Right $ Just (f c)

-- | Represents an empty network.
empty :: Network
empty = Network 0 0 M.empty M.empty

-- | Constructs a new cell with a given initial value and a function which
-- defines how to react if a new value is about to be written to the cell.
cell
  :: (CellKey a -> a)
  -- ^ Function which produces the initial value of the cell.
  -> (a -> a -> Change a)
  -- ^ A function that describes how to join an existing cell value with a
  -- new one that the cell has received via propagation.
  -> Propagator (CellKey a)
  -- ^ The identification of the newly constructed cell.
cell initValue f =
  Propagator $ \net ->
    let
      nextInt = nextRawCellKey net
      nextKey = CellKey nextInt
      newCell = Cell (initValue nextKey) f S.empty S.empty
      net' =
        net
          { nextRawCellKey = nextInt + 1
          , cells = M.insert nextInt (Some newCell) (cells net)
          }
    in
      Right (net', nextKey)

-- | Reads the value of a specific cell.
readCell :: CellKey a -> Propagator a
readCell k = value <$> getCell k

-- | Writes a new value to a specific cell and starts to propagate potential
-- changes through the network of connected cells.
writeCell :: a -> CellKey a -> Propagator ()
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
          Nothing ->
            Left (InvalidCell (Some ck))
          Just s@(Some someCell) ->
            let
              c@(Cell {value}) = unsafeCoerce someCell
            in
              case update c value newValue of
                Changed new ->
                  Right (subscribers c, Just (Some c { value = new }))
                Unchanged _ -> 
                  Right (S.empty, Just s)
                Incompatible ->
                  Left (Conflict (Some ck))

-- | Removes a cell from the network. This also removes all connections related to the cell.
removeCell :: CellKey a -> Propagator ()
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
label :: (a -> [b]) -> (b -> a) -> [CellKey a] -> Propagator [[b]]
label elems reify = solve [] . reverse
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

propagator :: ConnectState -> Prop -> Propagator ConnectKey
propagator state prop = do
  key@(ConnectKey k) <- addPropagator prop
  traverse_ (modifyCell $ addSub k) (sources prop)
  traverse_ (modifyCell $ addInc k) (targets prop)
  case state of
    Live -> fire key >> pure key
    Idle -> pure key
  where
    addSub k (Some c) = Some c { subscribers = S.insert k (subscribers c) }
    addInc k (Some c) = Some c { incomings = S.insert k (incomings c) }

-- | Connects a source cell to a target cell in order to propagate changes
-- from the source to the target. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
connect :: ConnectState -> CellKey a -> CellKey b -> (a -> Maybe b) -> Propagator ConnectKey
connect state source target f =
  propagator state $ Prop [Some source] [Some target] $
    \key -> do
      ins <- readCell source
      out <- toPropagator (NoPropagation key) (f ins)
      writeCell out target

-- | Same as 'connect', but discards the returned 'ConnectKey'.
connect_ :: ConnectState -> CellKey a -> CellKey b -> (a -> Maybe b) -> Propagator ()
connect_ state source target f =
  void $ connect state source target f

-- | Connects and synchronizes two cells, i.e. new values are propagated from
-- the source to the target cell, and vice versa. Short form of 'syncWith'
-- 'Just' 'Just'.
sync :: ConnectState -> CellKey a -> CellKey a -> Propagator (ConnectKey, ConnectKey)
sync = syncWith Just Just

-- | Same as 'sync', but discards the returned 'ConnectKey's.
sync_ :: ConnectState -> CellKey a -> CellKey a -> Propagator ()
sync_ state c1 c2 =
  void $ sync state c1 c2

-- | Connects and synchronizes two cells using two translation functions @f@
-- and @g@, i.e. new values are propagated from the source to the target cell
-- using @f@, and vice versa using @g@.
syncWith
  :: (a -> Maybe b)
  -> (b -> Maybe a)
  -> ConnectState
  -> CellKey a
  -> CellKey b
  -> Propagator (ConnectKey, ConnectKey)
syncWith f g state c1 c2 = do
  key1 <- connect state c1 c2 f
  key2 <- connect state c2 c1 g
  pure (key1, key2)

-- | Same as 'syncWith', but discards the returned 'ConnectKey's.
syncWith_
  :: (a -> Maybe b)
  -> (b -> Maybe a)
  -> ConnectState
  -> CellKey a
  -> CellKey b
  -> Propagator ()
syncWith_ f g state c1 c2 =
  void $ syncWith f g state c1 c2

-- | Connects two source cells to a target cell in order to propagate changes
-- from the sources to the target. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
combine :: ConnectState -> CellKey a -> CellKey b -> CellKey c -> (a -> b -> Maybe c) -> Propagator ConnectKey
combine state source1 source2 target f =
  propagator state $ Prop [Some source1, Some source2] [Some target] $
    \key -> do
      in1 <- readCell source1
      in2 <- readCell source2
      out <- toPropagator (NoPropagation key) (f in1 in2)
      writeCell out target

-- | Same as 'combine', but discards the returned 'ConnectKey'.
combine_ :: ConnectState -> CellKey a -> CellKey b -> CellKey c -> (a -> b -> Maybe c) -> Propagator ()
combine_ state source1 source2 target f =
  void $ combine state source1 source2 target f

-- | Connects several source cells to a target cell in order to propagate changes
-- from the sources to the target. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
combineMany :: ConnectState -> CellKeys ts -> CellKey a -> (CellValues ts -> Maybe a) -> Propagator ConnectKey
combineMany state sources target f =
  propagator state $ Prop (someKeys sources) [Some target] $
    \key -> do
      ins <- readValues sources
      out <- toPropagator (NoPropagation key) (f ins)
      writeCell out target

-- | Same as 'combineMany', but discards the returned 'ConnectKey'.
combineMany_ :: ConnectState -> CellKeys ts -> CellKey a -> (CellValues ts -> Maybe a) -> Propagator ()
combineMany_ state sources target f =
  void $ combineMany state sources target f

-- | Connects a source cells to several target cells in order to propagate changes
-- from the source to the targets. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
distribute :: ConnectState -> CellKey a -> [CellKey b] -> (a -> Maybe b) -> Propagator ConnectKey
distribute state source targets f =
  propagator state $ Prop [Some source] (fmap Some targets) $
    \key -> do
      ins <- readCell source
      out <- toPropagator (NoPropagation key) (f ins)
      traverse_ (writeCell out) targets

-- | Same as 'distribute', but discards the returned 'ConnectKey'.
distribute_ :: ConnectState -> CellKey a -> [CellKey b] -> (a -> Maybe b) -> Propagator ()
distribute_ state source targets f =
  void $ distribute state source targets f

-- | Connects several source cells to several target cells in order to propagate changes
-- from the sources to the targets. The returned 'ConnectKey' can be used to
-- remove the connection via 'disconnect'.
manyToMany :: ConnectState -> CellKeys xs -> [CellKey a] -> (CellValues xs -> Maybe a) -> Propagator ConnectKey
manyToMany state sources targets f =
  propagator state $ Prop (someKeys sources) (fmap Some targets) $
    \key -> do
      ins <- readValues sources
      out <- toPropagator (NoPropagation key) (f ins)
      traverse_ (writeCell out) targets

-- | Same as 'manyToMany', but discards the returned 'ConnectKey'.
manyToMany_ :: ConnectState -> CellKeys xs -> [CellKey a] -> (CellValues xs -> Maybe a) -> Propagator ()
manyToMany_ state sources targets f =
  void $ manyToMany state sources targets f

-- | Removes a connection from the network.
disconnect :: ConnectKey -> Propagator ()
disconnect key@(ConnectKey k) = do
  prop <- extractPropagator key
  traverse_ (modifyCell removeSub) (sources prop)
  traverse_ (modifyCell removeInc) (targets prop)
    where
      removeSub (Some c) =
        Some c { subscribers = S.delete k (subscribers c) }
      removeInc (Some c) =
        Some c { incomings = S.delete k (incomings c) }

fire :: ConnectKey -> Propagator ()
fire k = do
  prop <- getPropagator k
  action prop k

-- | Represents possible errors that may occur when modifying or using a network.
data Error
  = InvalidCell (Some CellKey)
  -- ^ The specified cell could not be found.
  | InvalidConnect ConnectKey
  -- ^ The specified connection could not be found.
  | NoPropagation ConnectKey
  -- ^ The specified connection did not produce a value.
  | Conflict (Some CellKey)
  -- ^ The old value of the specified cell is incompatible with a new value propagated to it.

toError :: Error -> Maybe a -> Either Error a
toError _ (Just a) = Right a
toError e Nothing  = Left e

toPropagator :: Error -> Maybe a -> Propagator a
toPropagator e m =
  case m of
    Just a  -> pure a
    Nothing -> failWith e

-- | @plus s a b c@ connects three cells using the following propagation schema:
--
-- * @a + b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @c - b@ is propagated to @a@ if @b@ or @c@ changes.
-- * @c - a@ is propagated to @b@ if @a@ or @c@ changes.
plus :: Num a => ConnectState -> CellKey a -> CellKey a -> CellKey a -> Propagator ()
plus state left right result = do
  combine_ state left right result (\lv rv -> Just (lv + rv))
  combine_ state left result right (\lv r  -> Just (r - lv))
  combine_ state right result left (\rv r  -> Just (r - rv))

-- | @minus s a b c@ connects three cells using the following propagation schema:
--
-- * @a - b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @b + c@ is propagated to @a@ if @b@ or @c@ changes.
-- * @a - c@ is propagated to @b@ if @a@ or @c@ changes.
minus :: Num a => ConnectState -> CellKey a -> CellKey a -> CellKey a -> Propagator ()
minus state left right result = do
  combine_ state left right result (\lv rv -> Just (lv - rv))
  combine_ state left result right (\lv r  -> Just (lv - r))
  combine_ state right result left (\rv r  -> Just (r + rv))

-- | @times s a b c@ connects three cells using the following propagation schema:
--
-- * @a * b@ is propagated to @c@ if @a@ or @b@ changes.
times :: Num a => ConnectState -> CellKey a -> CellKey a -> CellKey a -> Propagator ()
times state left right result =
  combine_ state left right result (\lv rv -> Just (lv * rv))

-- | @timesWith divOp s a b c@ connects three cells using the following propagation schema:
--
-- * @a * b@ is propagated to @c@ if @a@ or @b@ changes.
-- * @divOp c b@ is propagated to @a@ if @b@ or @c@ changes.
-- * @divOp c a@ is propagated to @b@ if @a@ or @c@ changes.
timesWith :: Num a => (a -> a -> a) -> ConnectState -> CellKey a -> CellKey a -> CellKey a -> Propagator ()
timesWith divOp state left right result = do
  times state left right result
  combine_ state left result right (\lv r -> Just (divOp r lv))
  combine_ state right result left (\rv r -> Just (divOp r rv))

-- | @abs s a b@ connects two cells using the following propagation schema:
--
-- * @|a|@ is propagated to @b@ if @a@ changes.
abs :: Num a => ConnectState -> CellKey a -> CellKey a -> Propagator ()
abs state left right =
  connect_ state left right (Just . Prelude.abs)

-- | @absWith inv s a b@ connects two cells using the following propagation schema:
--
-- * @|a|@ is propagated to @b@ if @a@ changes.
-- * @inv b@ is propagated to @a@ if @b@ changes.
absWith :: Num a => (a -> a) -> ConnectState -> CellKey a -> CellKey a -> Propagator ()
absWith inv state left right = do
  abs state left right
  connect_ state right left (Just . inv)

-- | @negate s a b@ connects two cells using the following propagation schema:
--
-- * @-a@ is propagated to @b@ if @a@ changes.
-- * @-b@ is propagated to @a@ if @b@ changes.
negate :: Num a => ConnectState -> CellKey a -> CellKey a -> Propagator ()
negate state left right = do
  connect_ state left right (Just . Prelude.negate)
  connect_ state right left (Just . Prelude.negate)

-- | @signum s a b@ connects two cells using the following propagation schema:
--
-- * @Prelude.signum a@ is propagated to @b@ if @a@ changes.
signum :: Num a => ConnectState -> CellKey a -> CellKey a -> Propagator ()
signum state left right =
  connect_ state left right (Just . Prelude.signum)

-- | @signumWith inv s a b@ connects two cells using the following propagation schema:
--
-- * @Prelude.signum a@ is propagated to @b@ if @a@ changes.
-- * @inv b@ is propagated to @a@ if @b@ changes.
signumWith :: Num a => (a -> a) -> ConnectState -> CellKey a -> CellKey a -> Propagator ()
signumWith inv state left right = do
  signum state left right
  connect_ state right left (Just . inv)