module MailStore 
(saveMail) where 

import Types
import Database.Redis
import Control.Monad.Trans
import Data.ByteString.UTF8 (toString, fromString)

-- bueno anda todo bastante bien
-- TODO: usar show/read para serializar/deserializar el mail
-- guardar el mail en una lista de redis usando como key el destinatario del mail
-- Hacer la funcion get que devuelve la lista de mails
-- Parametrizar el connectInfo en vez de usar el default
-- Loggear?
saveMail :: Mail -> IO ()
saveMail mail = do 
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    liftIO $ print "Getting record"
    liftIO $ print (readMailStr (show mail))
    liftIO $ print (from (readMailStr (show mail)))

readMailStr :: String -> Mail
readMailStr mailStr = read mailStr

getMail :: String -> [Mail]
getMail inbox = [Mail{from="lusho", to="nicky", content="hola nicky", sentTime=1234}, Mail{from="lusho", to="nicky", content="hola nicky", sentTime=1234}]