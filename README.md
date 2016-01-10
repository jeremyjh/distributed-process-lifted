##Overview
This Haskell library is intended to ease usage of monad transformer stacks with the Cloud Haskell `distributed-process` library. This package provides typeclasses and functions for lifting functions and control operations (such as `spawnLocal`) from the `Process` monad 
                     into transformer stacks based on the `Process` monad, in very similar way that the `lifted-base` package lifts IO functions into more generalized functions constrained to MonadIO and [MonadBaseControl IO](http://hackage.haskell.org/package/monad-control-1.0.0.1/docs/Control-Monad-Trans-Control.html#t:MonadBaseControl).

This library uses [MonadTransControl](http://hackage.haskell.org/package/monad-control-1.0.0.1/docs/Control-Monad-Trans-Control.html#t:MonadTransControl) and a new typeclass - `Control.Distributed.Process.Lifted.Class.MonadProcessBase` - which plays the same role as [MonadBaseControl IO](http://hackage.haskell.org/package/monad-control-1.0.0.1/docs/Control-Monad-Trans-Control.html#t:MonadBaseControl).  Instances are provided for all the [transformers](http://hackage.haskell.org/package/transformers) types - so stacks based on any of these (e.g. `ReaderT Config Process a`) can be used out-of-the-box.
                     
The `Control.Distributed.Process.Lifted` module exports all the same symbols as found in Control.Distributed.Process, but they are all generalized. Where appropriate it re-exports the more general functions from lifted-base (e.g. catch) rather than the versions re-implemented for `Process`.

##Example
For a motivation at a glance, consider the following two programs which perform the same function using the same monad stack, the first using lifted functions and the second using the base functions.

```haskell
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Node
import Control.Concurrent.Lifted (threadDelay)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

type MyConfig = String
type MyApp a = ReaderT MyConfig Process a

runMyApp :: LocalNode -> MyConfig -> MyApp () -> IO ()
runMyApp node conf ma = runProcess node (runReaderT ma conf)

useSomeConfig :: MyApp ()
useSomeConfig = ask >>= say

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runMyApp node "some config data" $ do
       parent <- getSelfPid
       spawnLocal $ do
         useSomeConfig
         send parent "all done"
       "all done" <- expect
       return ()
    threadDelay (50*1000)
```

And using the Base Process:

```haskell
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent.Lifted (threadDelay)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)

type MyConfig = String
type MyApp a = ReaderT MyConfig Process a

runMyApp :: LocalNode -> MyConfig -> MyApp () -> IO ()
runMyApp node conf ma = runProcess node (runReaderT ma conf)

withMyApp :: MyConfig -> MyApp a -> Process a
withMyApp conf ma = runReaderT ma conf

useSomeConfig :: MyApp ()
useSomeConfig = do
  config <- ask
  lift $ say config

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runMyApp node "some config data" $ do
       parent <- lift getSelfPid
       config <- ask
       lift $ spawnLocal $ do
         withMyApp config $ do
            useSomeConfig
            lift $ send parent "all done"
       "all done" <- lift expect
       return ()
    threadDelay (50*1000)
```