module MailStore 
(saveMail) where 

import Types

saveMail :: Mail -> IO ()
saveMail mail = do 
  print "Saving mail"
  print mail
  writeFile "hola.txt" "probando 1 2 3"

getMail :: String -> [Mail]
getMail inbox = [Mail{from="lusho", to="nicky", content="hola nicky", sentTime=1234}, Mail{from="lusho", to="nicky", content="hola nicky", sentTime=1234}]