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
