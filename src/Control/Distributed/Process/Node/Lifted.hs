{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Distributed.Process.Node.Lifted
   ( module Control.Distributed.Process.Node.Lifted
   , Base.LocalNode
   , Base.initRemoteTable
   , Base.localNodeId
   ) where


import Control.Monad.Base ( MonadBase, liftBase )
import Control.Monad (void)
import Control.Distributed.Process (ProcessId, Process, RemoteTable)
import Control.Distributed.Process.Node (LocalNode)

import Control.Exception (throw, SomeException)
import Control.Exception.Lifted (try)
import Network.Transport (Transport)
import qualified Control.Distributed.Process.Node as Base
import Control.Concurrent.MVar.Lifted
       (newEmptyMVar, putMVar, takeMVar)
import           Control.Distributed.Process.MonadBaseControl                     ()
import Control.DeepSeq (NFData, deepseq)


-- | Generalized version of 'MVar.putMVar'.
closeLocalNode ::  MonadBase IO m => LocalNode -> m ()
closeLocalNode = liftBase . Base.closeLocalNode

-- | Generalized version of 'Base.forkProcess'.
forkProcess :: MonadBase IO m => LocalNode -> Process () -> m ProcessId
forkProcess n = liftBase . Base.forkProcess n

-- | Generalized version of 'Base.newLocalNode'.
newLocalNode :: MonadBase IO m => Transport -> RemoteTable -> m LocalNode
newLocalNode t = liftBase . Base.newLocalNode t

-- | Generalized version of 'Base.runProcess'
runProcess :: MonadBase IO m => LocalNode -> Process () -> m ()
runProcess n = liftBase . Base.runProcess n

-- | A variant of 'runProcess' which returns a value. This works just
-- like 'runProcess' by forking a new process with a captured MVar, but it
-- will take the result of the computation. If the computation throws an
-- exception, it will be re-thrown by 'fromProcess' in the calling thread.
fromProcess :: forall a m. (NFData a, MonadBase IO m)
            => LocalNode -> Process a -> m a
fromProcess node ma =
 do resultMV <- newEmptyMVar
    void . forkProcess node $
     do eresult <- try (do a <- ma; a `deepseq` return a) :: Process (Either SomeException a)
        case eresult of
            Right result -> putMVar resultMV result
            Left exception -> putMVar resultMV (throw exception)
    !result <- takeMVar resultMV
    return result
