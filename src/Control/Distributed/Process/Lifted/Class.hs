{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Distributed.Process.Lifted.Class where

import           Control.Monad.Reader                                             (ReaderT)
import           Control.Monad.State                                              (StateT)
import qualified Control.Monad.State.Strict                                       as StateS
import           Control.Monad.Trans                                              (MonadIO,
                                                                                   lift)

import           Control.Monad.Trans.Control
import           Control.Distributed.Process.MonadBaseControl                     ()
import           Control.Monad.Base (MonadBase(..))
import Control.Distributed.Process      (Process)

-- | A class into instances of which Process operations can be lifted;
-- similar to MonadIO or MonadBase.
class (MonadIO m, MonadBase IO m, MonadBaseControl IO m) => MonadProcess m where
    -- |lift a base 'Process' computation into the current monad
    liftP :: Process a -> m a

-- | A Clone of 'MonadBaseControl' specialized to the Process monad. This
-- uses the MonadTransControl typeclass for transformer default instances, so the
-- core wrapping/unwrapping logic is not duplicated. This class
-- is needed because the MonadBaseControl instance for Process
-- has IO as the base, for interoperability (and because IO is in fact the
-- base monad for Process).
class (MonadProcess m) => MonadProcessBase m where
    type StMP m a :: *
    liftBaseWithP :: (RunInBaseP m -> Process a) -> m a
    restoreMP :: StMP m a -> m a

-- | A clone of 'RunInBase' for MonadProcessBase.
type RunInBaseP m = forall a. m a -> Process (StMP m a)

-- | A clone of 'ComposeSt' for MonadProcessBase.
type ComposeStP t m a = StMP m (StT t a)

-- | A clone of 'RunInBaseDefault' for MonadProcessBase.
type RunInBaseDefaultP t m = forall a. t m a -> Process (ComposeStP t m a)

-- | A clone of 'defaultLiftBaseWith' for MonadProcessBase.
-- This re-uses the MonadTransControl typeclass the same way as the
-- original; core wrapping/unwrapping logic for each transformer type is not duplicated.
defaultLiftBaseWithP :: (MonadTransControl t, MonadProcessBase m)
                     => (RunInBaseDefaultP t m -> Process a) -> t m a
defaultLiftBaseWithP f=  liftWith $ \run ->
                              liftBaseWithP $ \runInBase ->
                                f $ runInBase . run

-- | A clone of 'defaultRestoreMP' for MonadProcessBase.
-- This re-uses the MonadTransControl typeclass the same way as the
-- original; core wrapping/unwrapping logic for each transformer type is not duplicated.
defaultRestoreMP :: (MonadTransControl t, MonadProcessBase m)
                => ComposeStP t m a -> t m a
defaultRestoreMP = restoreT . restoreMP

-- | A clone of 'control' for MonadProcessBase.
controlP :: MonadProcessBase m => (RunInBaseP m -> Process (StMP m a)) -> m a
controlP f = liftBaseWithP f >>= restoreMP

-- | A clone of 'liftBaseDiscard' for MonadProcessBase.
liftBaseDiscardP :: MonadProcessBase m => (Process () -> Process a) -> m () -> m a
liftBaseDiscardP f m = liftBaseWithP $ \runInBase -> f $ StateS.void $ runInBase m

instance MonadProcess Process where
    liftP = id

instance MonadProcessBase Process where
    type StMP Process a = a
    liftBaseWithP f = f id
    restoreMP = return

instance (Monad m, MonadProcess m) => MonadProcess (ReaderT r m) where
    liftP = lift . liftP

instance MonadProcessBase m => MonadProcessBase (ReaderT r m) where
    type StMP (ReaderT r m) a = ComposeStP (ReaderT r) m a
    liftBaseWithP = defaultLiftBaseWithP
    restoreMP     = defaultRestoreMP

instance (Monad m, MonadProcess m) => MonadProcess (StateT s m) where
    liftP = lift . liftP

instance MonadProcessBase m => MonadProcessBase (StateT s m) where
    type StMP (StateT s m) a = ComposeStP (StateT s) m a
    liftBaseWithP = defaultLiftBaseWithP
    restoreMP     = defaultRestoreMP

instance (Monad m, MonadProcess m) => MonadProcess (StateS.StateT s m) where
    liftP = lift . liftP

instance MonadProcessBase m => MonadProcessBase (StateS.StateT s m) where
    type StMP (StateS.StateT s m) a = ComposeStP (StateS.StateT s) m a
    liftBaseWithP = defaultLiftBaseWithP
    restoreMP     = defaultRestoreMP
