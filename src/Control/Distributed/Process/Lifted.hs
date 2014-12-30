{-# Language RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Distributed.Process.Lifted
    ( module Control.Distributed.Process.Lifted
    , module Control.Distributed.Process
    , module Control.Exception.Lifted
    )
where

import Control.Distributed.Process.Lifted.Class


import Control.Distributed.Process
    (
      Closure
    , DidSpawn(..)
    , DiedReason(..)
    , Match
    , Message
    , MonitorRef
    , NodeId(..)
    , ProcessTerminationException(..)
    , ProcessRegistrationException(..)
    , ProcessLinkException(..)
    , NodeLinkException(..)
    , PortLinkException(..)
    , ProcessMonitorNotification(..)
    , NodeMonitorNotification(..)
    , PortMonitorNotification(..)
    , DiedReason(..)
    , NodeStats(..)
    , Process
    , ProcessId
    , ProcessInfo(..)
    , ReceivePort
    , RegisterReply(..)
    , RemoteTable
    , SendPort
    , SendPortId
    , SpawnRef
    , Static
    , WhereIsReply(..)
    , liftIO
    , unsafeWrapMessage
    , wrapMessage
    , closure
    , infoLinks
    , infoMessageQueueLength
    , infoMonitors
    , infoNode
    , infoRegisteredNames
    , isEncoded
    , nodeAddress
    , nodeStatsLinks
    , nodeStatsMonitors
    , nodeStatsNode
    , nodeStatsProcesses
    , nodeStatsRegisteredNames
    , processNodeId
    , sendPortId
    , sendPortProcessId
    , match
    , matchAnyIf
    , matchIf
    , matchAny
    , matchChan
    , matchMessage
    , matchMessageIf
    , matchSTM
    )
import qualified Control.Distributed.Process                                      as Base
import           Control.Distributed.Process.MonadBaseControl                     ()

import           Control.Distributed.Process.Serializable                         (Serializable)
import Control.Distributed.Process.Closure (SerializableDict)
import Control.Distributed.Process.Internal.Types
       (ProcessExitException(..))

import Control.Exception.Lifted
       (bracket, bracket_, catch, catches, Exception, finally, mask,
        mask_, onException, try, Handler(..))
import qualified Control.Exception.Lifted as EX
import Data.Typeable (Typeable)

-- compose arity 2 functions
(.:) :: (c->d) -> (a->b->c) -> a->b->d
f .: i = \l r -> f $ i l r

-- | Generalized version of 'Base.spawnLocal'
spawnLocal :: (MonadProcessBase m) => m () -> m ProcessId
spawnLocal = liftBaseDiscardP Base.spawnLocal

-- | Generalized version of 'Base.getSelfPid'
getSelfPid :: (MonadProcess m) => m ProcessId
getSelfPid = liftP Base.getSelfPid

-- | Generalized version of 'Base.expect'
expect :: (MonadProcess m) => forall a. Serializable a => m a
expect = liftP Base.expect

-- | Generalized version of 'Base.expectTimeout'
expectTimeout :: (MonadProcess m) => forall a. Serializable a => Int -> m (Maybe a)
expectTimeout = liftP . Base.expectTimeout

-- | Generalized version of 'Base.register'
register :: (MonadProcess m) => String -> ProcessId -> m ()
register name = liftP . Base.register name

-- | Generalized version of 'Base.whereis'
whereis :: (MonadProcess m) => String -> m (Maybe ProcessId)
whereis = liftP . Base.whereis

-- | Generalized version of 'Base.catchesExit'
catchesExit :: forall m b. (MonadProcessBase m) => m b -> [ProcessId -> Message -> m (Maybe b)] -> m b
-- TODO: This would be better than re-implementing this function, but I am not
-- smart enough to make it compile yet.
{-catchesExit ma handlers = controlP $ \runInP ->-}
                              {-let lifted = map (\handler ->-}
                                                    {-\pid msg -> runInP $ handler pid msg)-}
                                               {-handlers-}
                              {-in Base.catchesExit (runInP ma) lifted-}
catchesExit act handlers = catch act (`handleExit` handlers)
  where
    handleExit :: ProcessExitException
               -> [ProcessId -> Message -> m (Maybe b)]
               -> m b
    handleExit ex [] = EX.throwIO ex
    handleExit ex@(ProcessExitException from msg) (h:hs) = do
      r <- h from msg
      case r of
        Nothing -> handleExit ex hs
        Just p  -> return p

-- | Generalized version of 'Base.delegate'
delegate :: MonadProcess m => ProcessId -> (Message -> Bool) -> m ()
delegate = liftP .: Base.delegate

-- | Generalized version of 'Base.forward'
forward :: MonadProcess m => Message -> ProcessId -> m ()
forward = liftP .: Base.forward

-- | Generalized version of 'Base.getLocalNodeStats'
getLocalNodeStats :: MonadProcess m => m NodeStats
getLocalNodeStats = liftP Base.getLocalNodeStats

-- | Generalized version of 'Base.getNodeStats'
getNodeStats :: MonadProcess m => NodeId -> m (Either DiedReason NodeStats)
getNodeStats = liftP . Base.getNodeStats

-- | Generalized version of 'Base.getProcessInfo'
getProcessInfo :: MonadProcess m => ProcessId -> m (Maybe ProcessInfo)
getProcessInfo = liftP . Base.getProcessInfo

-- | Generalized version of 'Base.getSelfNode'
getSelfNode :: MonadProcess m => m NodeId
getSelfNode = liftP Base.getSelfNode

-- | Generalized version of 'Base.kill'
kill :: MonadProcess m => ProcessId -> String -> m ()
kill = liftP .: Base.kill

-- | Generalized version of 'Base.link'
link :: MonadProcess m => ProcessId -> m ()
link = liftP . Base.link

-- | Generalized version of 'Base.linkNode'
linkNode :: MonadProcess m => NodeId -> m ()
linkNode = liftP . Base.linkNode

-- | Generalized version of 'Base.linkPort'
linkPort :: MonadProcess m => SendPort a -> m ()
linkPort = liftP . Base.linkPort

-- | Generalized version of 'Base.monitor'
monitor :: MonadProcess m => ProcessId -> m MonitorRef
monitor = liftP . Base.monitor

-- | Generalized version of 'Base.monitorNode'
monitorNode :: MonadProcess m => NodeId -> m MonitorRef
monitorNode = liftP . Base.monitorNode

-- | Generalized version of 'Base.receiveTimeout'
receiveTimeout :: MonadProcess m => Int -> [Match b] -> m (Maybe b)
receiveTimeout = liftP .: Base.receiveTimeout

-- | Generalized version of 'Base.receiveWait'
receiveWait :: MonadProcess m => [Match b] -> m b
receiveWait = liftP . Base.receiveWait

-- | Generalized version of 'Base.reconnect'
reconnect :: MonadProcess m => ProcessId -> m ()
reconnect = liftP . Base.reconnect

-- | Generalized version of 'Base.reconnectPort'
reconnectPort :: MonadProcess m => SendPort a -> m ()
reconnectPort = liftP . Base.reconnectPort

-- | Generalized version of 'Base.registerRemoteAsync'
registerRemoteAsync :: MonadProcess m => NodeId -> String -> ProcessId -> m ()
registerRemoteAsync n = liftP .: Base.registerRemoteAsync n

-- | Generalized version of 'Base.relay'
relay :: MonadProcess m => ProcessId -> m ()
relay = liftP . Base.relay

-- | Generalized version of 'Base.reregister'
reregister :: MonadProcess m => String -> ProcessId -> m ()
reregister = liftP .: Base.reregister

-- | Generalized version of 'Base.reregisterRemoteAsync'
reregisterRemoteAsync :: MonadProcess m => NodeId -> String -> ProcessId -> m ()
reregisterRemoteAsync n = liftP .: Base.reregisterRemoteAsync n

-- | Generalized version of 'Base.say'
say :: MonadProcess m => String -> m ()
say = liftP . Base.say

-- | Generalized version of 'Base.spawn'
spawn :: MonadProcess m => NodeId -> Closure (Process ()) -> m ProcessId
spawn = liftP .: Base.spawn

-- | Generalized version of 'Base.spawnAsync'
spawnAsync :: MonadProcess m => NodeId -> Closure (Process ()) -> m SpawnRef
spawnAsync = liftP .: Base.spawnAsync

-- | Generalized version of 'Base.spawnLink'
spawnLink :: MonadProcess m => NodeId -> Closure (Process ()) -> m ProcessId
spawnLink = liftP .: Base.spawnLink

-- | Generalized version of 'Base.spawnMonitor'
spawnMonitor :: MonadProcess m => NodeId -> Closure (Process ()) -> m (ProcessId, MonitorRef)
spawnMonitor = liftP .: Base.spawnMonitor

-- | Generalized version of 'Base.spawnSupervised'
spawnSupervised :: MonadProcess m => NodeId -> Closure (Process ()) -> m (ProcessId, MonitorRef)
spawnSupervised = liftP .: Base.spawnSupervised

-- | Generalized version of 'Base.terminate'
terminate :: MonadProcess m => m a
terminate = liftP Base.terminate

-- | Generalized version of 'Base.unlink'
unlink :: MonadProcess m => ProcessId -> m ()
unlink = liftP . Base.unlink

-- | Generalized version of 'Base.unlinkNode'
unlinkNode :: MonadProcess m => NodeId -> m ()
unlinkNode = liftP . Base.unlinkNode

-- | Generalized version of 'Base.unlinkPort'
unlinkPort :: MonadProcess m => SendPort a -> m ()
unlinkPort = liftP . Base.unlinkPort

-- | Generalized version of 'Base.unmonitor'
unmonitor :: MonadProcess m => MonitorRef -> m ()
unmonitor = liftP . Base.unmonitor

-- | Generalized version of 'Base.unregister'
unregister :: MonadProcess m => String -> m ()
unregister = liftP . Base.unregister

-- | Generalized version of 'Base.unregisterRemoteAsync'
unregisterRemoteAsync :: MonadProcess m => NodeId -> String -> m ()
unregisterRemoteAsync = liftP .: Base.unregisterRemoteAsync

-- | Generalized version of 'Base.whereisRemoteAsync'
whereisRemoteAsync :: MonadProcess m => NodeId -> String -> m ()
whereisRemoteAsync = liftP .: Base.whereisRemoteAsync

-- | Generalized version of 'Base.withMonitor'
withMonitor :: MonadProcessBase m => ProcessId -> m a -> m a
withMonitor pid ma = controlP $ \runInP ->
                        Base.withMonitor pid (runInP ma)

-- | Generalized version of 'Base.call'
call :: (MonadProcess m,Serializable a)
     => Static (SerializableDict a) -> NodeId -> Closure (Process a) -> m a
call s = liftP .: Base.call s

-- | Generalized version of 'Base.catchExit'
catchExit :: (MonadProcessBase m,Show a,Serializable a)
          => m b -> (ProcessId -> a -> m b) -> m b
catchExit ma handler = controlP $ \runInP ->
                           Base.catchExit (runInP ma)
                                          (\pid msg -> runInP $ handler pid msg)

-- | Generalized version of 'Base.die'
die :: (MonadProcess m, Serializable a) => a -> m b
die = liftP . Base.die

-- | Generalized version of 'Base.exit'
exit :: (MonadProcess m, Serializable a) => ProcessId -> a -> m ()
exit = liftP .: Base.exit

-- | Generalized version of 'Base.handleMessage'
handleMessage :: (MonadProcess m,Serializable a)
              => Message -> (a -> Process b) -> m (Maybe b)
handleMessage msg f = liftP $ Base.handleMessage msg f

-- | Generalized version of 'Base.handleMessageIf'
handleMessageIf :: (MonadProcess m,Serializable a)
                => Message -> (a -> Bool) -> (a -> Process b) -> m (Maybe b)
handleMessageIf msg p f = liftP $ Base.handleMessageIf msg p f

-- | Generalized version of 'Base.handleMessageIf_'
handleMessageIf_ :: (MonadProcess m,Serializable a)
                 => Message -> (a -> Bool) -> (a -> Process ()) -> m ()
handleMessageIf_ msg p f = liftP $ Base.handleMessageIf_ msg p f

-- | Generalized version of 'Base.handleMessage_'
handleMessage_ :: (MonadProcess m, Serializable a) => Message -> (a -> Process ()) -> m ()
handleMessage_ msg f = liftP $ Base.handleMessage_ msg f

-- | Generalized version of 'Base.mergePortsBiased'
mergePortsBiased :: (MonadProcess m,Serializable a)
                 => [ReceivePort a] -> m (ReceivePort a)
mergePortsBiased = liftP . Base.mergePortsBiased

-- | Generalized version of 'Base.mergePortsRR'
mergePortsRR :: (MonadProcess m, Serializable a) => [ReceivePort a] -> m (ReceivePort a)
mergePortsRR = liftP . Base.mergePortsRR

-- | Generalized version of 'Base.monitorPort'
monitorPort :: (MonadProcess m, Serializable a) => SendPort a -> m MonitorRef
monitorPort = liftP . Base.monitorPort

-- | Generalized version of 'Base.newChan'
newChan :: (MonadProcess m, Serializable a) => m (SendPort a, ReceivePort a)
newChan = liftP Base.newChan

-- | Generalized version of 'Base.nsend'
nsend :: (MonadProcess m, Serializable a) => String -> a -> m ()
nsend = liftP .: Base.nsend

-- | Generalized version of 'Base.nsendRemote'
nsendRemote :: (MonadProcess m, Serializable a) => NodeId -> String -> a -> m ()
nsendRemote n = liftP .: Base.nsendRemote n

-- | Generalized version of 'Base.proxy'
proxy :: (MonadProcess m, Serializable a) => ProcessId -> (a -> Process Bool) -> m ()
proxy = liftP .: Base.proxy

-- | Generalized version of 'Base.receiveChan'
receiveChan :: (MonadProcess m, Serializable a) => ReceivePort a -> m a
receiveChan = liftP . Base.receiveChan

-- | Generalized version of 'Base.receiveChanTimeout'
receiveChanTimeout :: (MonadProcess m, Serializable a) => Int -> ReceivePort a -> m (Maybe a)
receiveChanTimeout = liftP .: Base.receiveChanTimeout

-- | Generalized version of 'Base.send'
send :: (MonadProcess m, Serializable a) => ProcessId -> a -> m ()
send = liftP .: Base.send

-- | Generalized version of 'Base.sendChan'
sendChan :: (MonadProcess m, Serializable a) => SendPort a -> a -> m ()
sendChan = liftP .: Base.sendChan

-- | Generalized version of 'Base.spawnChannel'
spawnChannel :: (MonadProcess m,Serializable a)
             => Static (SerializableDict a)
             -> NodeId
             -> Closure (ReceivePort a -> Process ())
             -> m (SendPort a)
spawnChannel s = liftP .: Base.spawnChannel s

-- | Generalized version of 'Base.spawnChannelLocal'
spawnChannelLocal :: (MonadProcess m,Serializable a)
                  => (ReceivePort a -> Process ()) -> m (SendPort a)
spawnChannelLocal = liftP . Base.spawnChannelLocal

-- | Generalized version of 'Base.unClosure'
unClosure :: (MonadProcess m, Typeable a) => Closure a -> m a
unClosure = liftP . Base.unClosure

-- | Generalized version of 'Base.unStatic'
unStatic :: (MonadProcess m, Typeable a) => Static a -> m a
unStatic = liftP . Base.unStatic

-- | Generalized version of 'Base.unsafeNSend'
unsafeNSend :: (MonadProcess m, Serializable a) => String -> a -> m ()
unsafeNSend = liftP .: Base.unsafeNSend

-- | Generalized version of 'Base.unsafeSend'
unsafeSend :: (MonadProcess m, Serializable a) => ProcessId -> a -> m ()
unsafeSend = liftP .: Base.unsafeSend

-- | Generalized version of 'Base.unsafeSendChan'
unsafeSendChan :: (MonadProcess m, Serializable a) => SendPort a -> a -> m ()
unsafeSendChan = liftP .: Base.unsafeSendChan

-- | Generalized version of 'Base.unwrapMessage'
unwrapMessage :: (MonadProcess m, Serializable a) => Message -> m (Maybe a)
unwrapMessage = liftP . Base.unwrapMessage
