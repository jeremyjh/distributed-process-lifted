{-# LANGUAGE FlexibleContexts #-}

module Control.Distributed.Process.Lifted.Class where

import           Control.Monad.Reader                                             (ReaderT, mapReaderT)
import           Control.Monad.State                                              (StateT, mapStateT)
import qualified Control.Monad.State                                              as State
import           Control.Monad.Trans                                              (MonadIO,
                                                                                   lift)

import           Control.Monad.Trans.Control                                      (MonadBaseControl (..))
import           Control.Distributed.Process.MonadBaseControl                     ()
import Control.Distributed.Process      (Process)
import           Data.Tuple                                                       (swap)

-- lifted versions of Process functions
class (MonadIO m, MonadBaseControl IO m) => MonadProcess m where
    -- |lift a base 'Process' computation into the current monad
    liftP :: Process a -> m a

class (MonadProcess m) => MonadProcessBase m where
    -- |map over an underlying Process to e.g. lift spawnLocal
    mapProcess :: (Process a -> Process b) -> m a -> m b

instance MonadProcess Process where
    liftP = id

instance MonadProcessBase Process where
    mapProcess = id

instance (Monad m, MonadProcess m) => MonadProcess (ReaderT r m) where
    liftP = lift . liftP

instance MonadProcessBase m => MonadProcessBase (ReaderT r m) where
    mapProcess f = mapReaderT (mapProcess f)

instance (Monad m, MonadProcess m) => MonadProcess (StateT s m) where
    liftP = lift . liftP

instance MonadProcessBase m => MonadProcessBase (StateT s m) where
    mapProcess f ma =
     do s <- State.get
        flip mapStateT ma $ \ mas ->
            let stripped = fmap fst mas
                applied = mapProcess f stripped
                tack = swap . (,) s
            in fmap tack applied
