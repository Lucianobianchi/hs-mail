module MailStore 
(saveMail) where 

import Types
import Database.Redis
import Control.Monad.Trans
import Data.ByteString.UTF8 (toString, fromString)

readMail :: String -> Mail
readMail mail = read mail

-- TODO:
-- handle para errores
-- Parametrizar el connectInfo en vez de usar el default
saveMail :: Mail -> IO ()
saveMail mail = do 
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    liftIO $ print $ "Saving email: " ++ show mail
    rpush (fromString $ to mail) [fromString $ show mail]
    return ()

-- TODO: medio raro el tipo de esto, no se como pasarle el handler
getMails :: String -> ([Mail] -> Redis ()) -> IO () 
getMails inbox handle = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    liftIO $ print $ "Retrieving emails for: " ++ inbox
    result <- lrange (fromString $ inbox) 0 (-1) -- to get the whole list
    case result of 
      Right allmails -> handle (map (readMail.toString) allmails)
      Left err -> liftIO $ print err