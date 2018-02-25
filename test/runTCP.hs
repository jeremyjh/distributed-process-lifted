-- Run tests using the TCP transport.

module Main where

import Control.Distributed.Process.Lifted.Tests (tests)

import Network.Transport.Test (TestTransport(..))
import Network.Socket (close)
import Network.Transport.TCP
  ( createTransportExposeInternals
  , TransportInternals(socketBetween)
  , defaultTCPParameters
  )
import Test.Framework (defaultMainWithArgs)

import Control.Concurrent (threadDelay)
import System.Environment (getArgs)

main :: IO ()
main = do
    Right (transport, internals) <-
      createTransportExposeInternals "127.0.0.1"
                                     "8080"
                                     (\s -> ("127.0.0.1", s))
                                     defaultTCPParameters
    ts <- tests TestTransport
      { testTransport = transport
      , testBreakConnection = \addr1 addr2 -> do
          sock <- socketBetween internals addr1 addr2
          close sock
          threadDelay 10000
      }
    args <- getArgs
    -- Tests are time sensitive. Running the tests concurrently can slow them
    -- down enough that threads using threadDelay would wake up later than
    -- expected, thus changing the order in which messages were expected.
    -- Therefore we run the tests sequentially by passing "-j 1" to
    -- test-framework. This does not solve the issue but makes it less likely.
    --
    -- The problem was first detected with
    -- 'Control.Distributed.Process.Tests.CH.testMergeChannels'
    -- in particular.
    defaultMainWithArgs ts ("-j" : "1" : args)
