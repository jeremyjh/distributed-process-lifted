{-# LANGUAGE FlexibleContexts #-}

module Control.Distributed.Process.Node.Lifted
   ( module Control.Distributed.Process.Node.Lifted
   , Base.LocalNode
   , Base.initRemoteTable
   , Base.localNodeId
   ) where


import Control.Monad.Base ( MonadBase, liftBase )
import Control.Distributed.Process (ProcessId, Process, RemoteTable)
import Control.Distributed.Process.Node (LocalNode)

import Network.Transport (Transport)
import qualified Control.Distributed.Process.Node as Base
import           Control.Distributed.Process.MonadBaseControl                     ()


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
