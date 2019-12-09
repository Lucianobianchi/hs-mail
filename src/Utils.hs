module Utils 
(send, sendMany, isValidEmail) where

import Data.ByteString.UTF8 (fromString)
import Network.Socket.ByteString (sendAll)
import Network.Socket
import Text.Email.Validate
import SMTP.Types

send :: Socket -> String -> IO ()
send socket msg = sendAll socket $ fromString (msg ++ "\n")

sendMany :: Socket -> [String] -> IO ()
sendMany socket [] = return ()
sendMany socket (m:ms) = do
  send socket m
  sendMany socket ms

isValidEmail :: String -> Bool 
isValidEmail em = isValid (fromString em)
