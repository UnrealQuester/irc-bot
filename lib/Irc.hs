{-# LANGUAGE OverloadedStrings #-}
module Irc(runIrcClient) where

import Data.Conduit
import Control.Concurrent.Async (concurrently)
import Control.Monad            (void)
import Data.Conduit.Network
import Data.Conduit.Attoparsec
import Data.Conduit.Text (encode, decode, utf8)
import IrcParser
import Control.Monad.Trans.Class (lift)
import Data.Text

debug :: Show a => Conduit a IO a
debug = do
    awaitForever $ \l -> do
        (lift . print) l
        yield l

runIrcClient :: IO ()
runIrcClient = runTCPClient (clientSettings 6667 "192.168.33.10") $ \server ->
    void $ concurrently
        ((yield "Nick asdf\nUSER asdf 0 * :asdf\n") $$ appSink server)
        (appSource server $= decode utf8 =$= debug =$= parseMessage =$= debug =$= action =$= encode utf8 $$ appSink server)

parseMessage :: Conduit Text IO IrcEvent
parseMessage = do
    conduitParser parseIrc =$= awaitForever go
    where
        go (_, irc) = yield irc

action :: Conduit IrcEvent IO Text
action = awaitForever $ \m ->
        yield ":asdf JOIN #technik\n"
