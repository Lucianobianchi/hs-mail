module Main (main) where

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.Thread (wait)
import qualified Control.Exception as E
import Network.Socket
import Control.Monad (unless, forever, void)
import SMTP.Server(smtpServer)
import POP.Server(popServer)

main :: IO ()
main = do 
  smtpThreadId <- forkIO $ do 
    runTCPServer Nothing "25" smtpServer
    return ()
  -- TODO: ver como correr los dos en paralelo y bloquear la ejecución hasta que ambos terminen
  popThreadId <- forkIO $ do 
    runTCPServer Nothing "100" popServer
    return ()
  return ()

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