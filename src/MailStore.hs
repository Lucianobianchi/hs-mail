module MailStore 
(saveMail, getMails, deleteMail) where 

import Types
import Database.Redis
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.UTF8 (toString, fromString)

-- TODO: case insensitiveness!!!

readMail :: String -> Mail
readMail mail = read mail

-- TODO: handle para errores
saveMail :: Mail -> IO ()
saveMail mail = do 
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    liftIO $ print $ "Saving email: " ++ show mail
    forM (to mail) $ (\recipient -> do
      rpush (fromString recipient) [fromString $ show mail])
    return ()

getMails :: String -> ([Mail] -> IO a) -> IO a
getMails inbox handle = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    liftIO $ print $ "Retrieving emails for: " ++ inbox
    result <- lrange (fromString inbox) 0 (-1) -- to get the whole list
    case result of 
      Right allmails -> liftIO $ handle (map (readMail.toString) allmails)
      Left err -> liftIO $ handle []

deleteMail :: String -> Integer -> IO ()
deleteMail inbox index = do 
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    lset (fromString inbox) index (fromString "__DELETED__")
    lrem (fromString inbox) 1 (fromString "__DELETED__")
    liftIO $ print $ "Deleting email at inbox " ++ inbox ++ " index " ++ (show index)
    return ()