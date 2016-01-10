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
