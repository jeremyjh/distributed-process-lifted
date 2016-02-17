{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}


-- | Utility functions for working with Processes outside of the
-- 'Process' monad.
module Control.Distributed.Process.Lifted.Extras
    ( fromProcess
    , ProcessProxy
    , proxyPid
    , spawnProxy
    , spawnProxyIO
    , inProxy
    , fromProxy
    )
where

import Control.Monad (void, join, forever)
import Control.Monad.Base (MonadBase(..))
import Control.Exception (throw, SomeException)
import Data.Typeable (Typeable)

import Control.Concurrent.Chan.Lifted
       (Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar.Lifted
       (newEmptyMVar, putMVar, takeMVar)

import Control.Distributed.Process.Lifted hiding (newChan)
import Control.Distributed.Process.Node.Lifted (LocalNode, forkProcess)


-- | A variant of 'Control.Distributed.Process.Node.runProcess' which returns a value. This works just
-- like 'Control.Distributed.Process.Node.runProcess' by forking a new process with a captured 'Control.Concurrent.MVar.MVar', but it
-- will return the result of the computation. If the computation throws an
-- exception, it will be re-thrown by 'fromProcess' in the calling thread.
fromProcess :: forall a m. (MonadBase IO m)
            => LocalNode -> Process a -> m a
fromProcess node ma =
 do resultMV <- newEmptyMVar
    void . forkProcess node $
     do eresult <- try (do !a <- ma; return a) :: Process (Either SomeException a)
        case eresult of
            Right result -> putMVar resultMV result
            Left exception -> putMVar resultMV (throw exception)
    !result <- takeMVar resultMV
    return result

-- | Represents a handle to a process runner that communicates
-- through a 'Control.Concurrent.Chan.Chan'.
-- Create with 'spawnProxy' or 'spawnProxyIO'.
-- Use this to call process actions (using 'fromProxy' or 'inProxy') from any IO
-- that will be executed in a single process that will have a
-- persistent pid and mailbox across invocations.
-- Sharing a single proxy between threads may yield poor performance and is not advised.
data ProcessProxy = ProcessProxy {
     proxyPid  :: !ProcessId,
     proxyChan :: !(Chan (Process ()))
} deriving (Typeable)

instance Show ProcessProxy where
    show = show . proxyPid

-- | Spawn a new process and return a 'ProcessProxy' handle for it.
spawnProxy :: Process ProcessProxy
spawnProxy =
 do action <- newChan
    pid <- spawnLocal . forever $
        join (readChan action)
    return (ProcessProxy pid action)

-- | Same as spawnProxy but can be used from any IO
--
-- spawnProxyIO node = fromProcess node spawnProxy
spawnProxyIO :: forall m. (MonadBase IO m)
             => LocalNode -> m ProcessProxy
spawnProxyIO node = fromProcess node spawnProxy

-- | Use a 'ProcessProxy' created with 'spawnProxy' to run a
-- Process computation in the existing Process asynchronously.
inProxy :: forall m. (MonadBase IO m)
          => ProcessProxy -> Process () -> m ()
inProxy = writeChan . proxyChan

-- | Use a 'ProcessProxy' created with 'spawnProxy' to run a
-- Process computation in the existing Process and return the result
-- in any IO.
fromProxy :: forall a m. (MonadBase IO m)
          => ProcessProxy -> Process a -> m a
fromProxy (ProcessProxy _ prox) ma =
 do result <- newEmptyMVar
    writeChan prox (ma >>= putMVar result)
    takeMVar result
