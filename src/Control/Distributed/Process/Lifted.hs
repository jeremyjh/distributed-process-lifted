{-# LANGUAGE RankNTypes #-}

module Control.Distributed.Process.Lifted
    ( module Control.Distributed.Process.Lifted
    , module Control.Distributed.Process
    , module Control.Exception.Lifted
    )
where

import Control.Distributed.Process.Lifted.Class


import Control.Distributed.Process
    (Closure
    , DidSpawn(..)
    , DiedReason(..)
    , Handler(..)
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
    -- | Functions which do not run in Process and thus do not need to
    -- be lifted.
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

import Control.Exception.Lifted
       (bracket, bracket_, catch, catches, Exception, finally
       ,mask, mask_, onException, try)
import Data.Typeable (Typeable)

-- compose arity 2 functions
(.:) :: (c->d) -> (a->b->c) -> a->b->d
f .: i = \l r -> f $ i l r

spawnLocal :: (MonadProcess m) => m () -> m ProcessId
spawnLocal = mapProcess Base.spawnLocal

getSelfPid :: (MonadProcess m) => m ProcessId
getSelfPid = liftP Base.getSelfPid

expect :: (MonadProcess m) => forall a. Serializable a => m a
expect = liftP Base.expect

expectTimeout :: (MonadProcess m) => forall a. Serializable a => Int -> m (Maybe a)
expectTimeout = liftP . Base.expectTimeout

register :: (MonadProcess m) => String -> ProcessId -> m ()
register name = liftP . Base.register name

whereis :: (MonadProcess m) => String -> m (Maybe ProcessId)
whereis = liftP . Base.whereis

catchesExit :: MonadProcess m => m b -> [ProcessId -> Message -> Process (Maybe b)] -> m b
catchesExit m f = mapProcess (`Base.catchesExit` f) m


delegate :: MonadProcess m => ProcessId -> (Message -> Bool) -> m ()
delegate = liftP .: Base.delegate

forward :: MonadProcess m => Message -> ProcessId -> m ()
forward = liftP .: Base.forward

getLocalNodeStats :: MonadProcess m => m NodeStats
getLocalNodeStats = liftP Base.getLocalNodeStats

getNodeStats :: MonadProcess m => NodeId -> m (Either DiedReason NodeStats)
getNodeStats = liftP . Base.getNodeStats

getProcessInfo :: MonadProcess m => ProcessId -> m (Maybe ProcessInfo)
getProcessInfo = liftP . Base.getProcessInfo

getSelfNode :: MonadProcess m => m NodeId
getSelfNode = liftP Base.getSelfNode

kill :: MonadProcess m => ProcessId -> String -> m ()
kill = liftP .: Base.kill

link :: MonadProcess m => ProcessId -> m ()
link = liftP . Base.link

linkNode :: MonadProcess m => NodeId -> m ()
linkNode = liftP . Base.linkNode

linkPort :: MonadProcess m => SendPort a -> m ()
linkPort = liftP . Base.linkPort

monitor :: MonadProcess m => ProcessId -> m MonitorRef
monitor = liftP . Base.monitor

monitorNode :: MonadProcess m => NodeId -> m MonitorRef
monitorNode = liftP . Base.monitorNode

receiveTimeout :: MonadProcess m => Int -> [Match b] -> m (Maybe b)
receiveTimeout = liftP .: Base.receiveTimeout

receiveWait :: MonadProcess m => [Match b] -> m b
receiveWait = liftP . Base.receiveWait

reconnect :: MonadProcess m => ProcessId -> m ()
reconnect = liftP . Base.reconnect

reconnectPort :: MonadProcess m => SendPort a -> m ()
reconnectPort = liftP . Base.reconnectPort

registerRemoteAsync :: MonadProcess m => NodeId -> String -> ProcessId -> m ()
registerRemoteAsync n = liftP .: Base.registerRemoteAsync n

relay :: MonadProcess m => ProcessId -> m ()
relay = liftP . Base.relay

reregister :: MonadProcess m => String -> ProcessId -> m ()
reregister = liftP .: Base.reregister

reregisterRemoteAsync :: MonadProcess m => NodeId -> String -> ProcessId -> m ()
reregisterRemoteAsync n = liftP .: Base.reregisterRemoteAsync n

say :: MonadProcess m => String -> m ()
say = liftP . Base.say

spawn :: MonadProcess m => NodeId -> Closure (Process ()) -> m ProcessId
spawn = liftP .: Base.spawn

spawnAsync :: MonadProcess m => NodeId -> Closure (Process ()) -> m SpawnRef
spawnAsync = liftP .: Base.spawnAsync

spawnLink :: MonadProcess m => NodeId -> Closure (Process ()) -> m ProcessId
spawnLink = liftP .: Base.spawnLink

spawnMonitor :: MonadProcess m => NodeId -> Closure (Process ()) -> m (ProcessId, MonitorRef)
spawnMonitor = liftP .: Base.spawnMonitor

spawnSupervised :: MonadProcess m => NodeId -> Closure (Process ()) -> m (ProcessId, MonitorRef)
spawnSupervised = liftP .: Base.spawnSupervised

terminate :: MonadProcess m => m a
terminate = liftP Base.terminate

unlink :: MonadProcess m => ProcessId -> m ()
unlink = liftP . Base.unlink

unlinkNode :: MonadProcess m => NodeId -> m ()
unlinkNode = liftP . Base.unlinkNode

unlinkPort :: MonadProcess m => SendPort a -> m ()
unlinkPort = liftP . Base.unlinkPort

unmonitor :: MonadProcess m => MonitorRef -> m ()
unmonitor = liftP . Base.unmonitor

unregister :: MonadProcess m => String -> m ()
unregister = liftP . Base.unregister

unregisterRemoteAsync :: MonadProcess m => NodeId -> String -> m ()
unregisterRemoteAsync = liftP .: Base.unregisterRemoteAsync

whereisRemoteAsync :: MonadProcess m => NodeId -> String -> m ()
whereisRemoteAsync = liftP .: Base.whereisRemoteAsync

withMonitor :: MonadProcess m => ProcessId -> m a -> m a
withMonitor pid = mapProcess $ Base.withMonitor pid

call :: (MonadProcess m, Serializable a) => Static (SerializableDict a) -> NodeId -> Closure (Process a) -> m a
call s = liftP .: Base.call s

catchExit :: (MonadProcess m, Show a, Serializable a) => m b -> (ProcessId -> a -> Process b) -> m b
catchExit m f = mapProcess (`Base.catchExit` f) m

die :: (MonadProcess m, Serializable a) => a -> m b
die = liftP . Base.die

exit :: (MonadProcess m, Serializable a) => ProcessId -> a -> m ()
exit = liftP .: Base.exit

handleMessage :: (MonadProcess m, Serializable a) => Message -> (a -> Process b) -> m (Maybe b)
handleMessage msg f = liftP $ Base.handleMessage msg f

handleMessageIf :: (MonadProcess m, Serializable a) => Message -> (a -> Bool) -> (a -> Process b) -> m (Maybe b)
handleMessageIf msg p f = liftP $ Base.handleMessageIf msg p f

handleMessageIf_ :: (MonadProcess m, Serializable a) => Message -> (a -> Bool) -> (a -> Process ()) -> m ()
handleMessageIf_ msg p f = liftP $ Base.handleMessageIf_ msg p f

handleMessage_ :: (MonadProcess m, Serializable a) => Message -> (a -> Process ()) -> m ()
handleMessage_ msg f = liftP $ Base.handleMessage_ msg f

mergePortsBiased :: (MonadProcess m, Serializable a) => [ReceivePort a] -> m (ReceivePort a)
mergePortsBiased = liftP . Base.mergePortsBiased

mergePortsRR :: (MonadProcess m, Serializable a) => [ReceivePort a] -> m (ReceivePort a)
mergePortsRR = liftP . Base.mergePortsRR

monitorPort :: (MonadProcess m, Serializable a) => SendPort a -> m MonitorRef
monitorPort = liftP . Base.monitorPort

newChan :: (MonadProcess m, Serializable a) => m (SendPort a, ReceivePort a)
newChan = liftP Base.newChan

nsend :: (MonadProcess m, Serializable a) => String -> a -> m ()
nsend = liftP .: Base.nsend

nsendRemote :: (MonadProcess m, Serializable a) => NodeId -> String -> a -> m ()
nsendRemote n = liftP .: Base.nsendRemote n

proxy :: (MonadProcess m, Serializable a) => ProcessId -> (a -> Process Bool) -> m ()
proxy = liftP .: Base.proxy

receiveChan :: (MonadProcess m, Serializable a) => ReceivePort a -> m a
receiveChan = liftP . Base.receiveChan

receiveChanTimeout :: (MonadProcess m, Serializable a) => Int -> ReceivePort a -> m (Maybe a)
receiveChanTimeout = liftP .: Base.receiveChanTimeout

send :: (MonadProcess m, Serializable a) => ProcessId -> a -> m ()
send = liftP .: Base.send

sendChan :: (MonadProcess m, Serializable a) => SendPort a -> a -> m ()
sendChan = liftP .: Base.sendChan

spawnChannel :: (MonadProcess m, Serializable a) => Static (SerializableDict a) -> NodeId -> Closure (ReceivePort a -> Process ()) -> m (SendPort a)
spawnChannel s = liftP .: Base.spawnChannel s

spawnChannelLocal :: (MonadProcess m, Serializable a) => (ReceivePort a -> Process ()) -> m (SendPort a)
spawnChannelLocal = liftP . Base.spawnChannelLocal

unClosure :: (MonadProcess m, Typeable a) => Closure a -> m a
unClosure = liftP . Base.unClosure

unStatic :: (MonadProcess m, Typeable a) => Static a -> m a
unStatic = liftP . Base.unStatic

unsafeNSend :: (MonadProcess m, Serializable a) => String -> a -> m ()
unsafeNSend = liftP .: Base.unsafeNSend

unsafeSend :: (MonadProcess m, Serializable a) => ProcessId -> a -> m ()
unsafeSend = liftP .: Base.unsafeSend

unsafeSendChan :: (MonadProcess m, Serializable a) => SendPort a -> a -> m ()
unsafeSendChan = liftP .: Base.unsafeSendChan

unwrapMessage :: (MonadProcess m, Serializable a) => Message -> m (Maybe a)
unwrapMessage = liftP . Base.unwrapMessage
