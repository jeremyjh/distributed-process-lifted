{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Control.Distributed.Process.Lifted.Class where

import Control.Distributed.Process      (Process)
import           Control.Distributed.Process.MonadBaseControl                     ()

import qualified Control.Monad.State.Strict                                       as StateS
import           Control.Monad.Trans                                              (MonadIO,
                                                                                   lift)

import           Control.Monad.Trans.Control
import           Control.Monad.Base (MonadBase(..))

import Data.Monoid ( Monoid )

import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Trans.RWS (RWST)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)

-- | A class into instances of which Process operations can be lifted;
-- similar to MonadIO or MonadBase.
class (Monad m, MonadIO m, MonadBase IO m, MonadBaseControl IO m) => MonadProcess m where
    -- |lift a base 'Process' computation into the current monad
    liftP :: Process a -> m a

-- | A Clone of 'MonadBaseControl' specialized to the Process monad. This
-- uses the 'MonadTransControl' typeclass for transformer default instances, so the
-- core wrapping/unwrapping logic is not duplicated. This class
-- is needed because the MonadBaseControl instance for Process
-- has IO as the base.
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

#define LIFTP(T) \
instance (MonadProcess m) => MonadProcess (T m) where liftP = lift . liftP \

LIFTP(IdentityT)
LIFTP(MaybeT)
LIFTP(ListT)
LIFTP(ReaderT r)
LIFTP(Strict.StateT s)
LIFTP( StateT s)
LIFTP(ExceptT e)

#undef LIFTP
#define LIFTP(CTX, T) \
instance (CTX, MonadProcess m) => MonadProcess (T m) where liftP = lift . liftP \

LIFTP(Monoid w, Strict.WriterT w)
LIFTP(Monoid w, WriterT w)
LIFTP(Monoid w, Strict.RWST r w s)
LIFTP(Monoid w, RWST r w s)

#define BODY(T) { \
    type StMP (T m) a = ComposeStP (T) m a; \
    liftBaseWithP = defaultLiftBaseWithP; \
    restoreMP = defaultRestoreMP; \
    {-# INLINABLE liftBaseWithP #-}; \
    {-# INLINABLE restoreMP #-}}

#define TRANS( T) \
    instance (MonadProcessBase m) => MonadProcessBase (T m) where BODY(T)
#define TRANS_CTX(CTX, T) \
    instance (CTX, MonadProcessBase m) => MonadProcessBase (T m) where BODY(T)

TRANS(IdentityT)
TRANS(MaybeT)
TRANS(ListT)
TRANS(ReaderT r)
TRANS(Strict.StateT s)
TRANS( StateT s)
TRANS(ExceptT e)
TRANS_CTX(Monoid w, Strict.WriterT w)
TRANS_CTX(Monoid w, WriterT w)
TRANS_CTX(Monoid w, Strict.RWST r w s)
TRANS_CTX(Monoid w, RWST r w s)
