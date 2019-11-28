module Main (main) where

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Network.Socket
import Control.Monad (unless, forever, void)
import SMTP.Server(smtpServer)
import POP.Server(popServer)

smtpRun = runTCPServer Nothing "25" smtpServer
popRun = runTCPServer Nothing "110" popServer

main = do
  smtpMvar <- newEmptyMVar
  popMvar <- newEmptyMVar
  forkFinally smtpRun (\_ -> putMVar smtpMvar ())
  forkFinally popRun (\_ -> putMVar popMvar ())
  readMVar smtpMvar
  readMVar popMvar

-- TODO: entender mejor que hace esto
-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)