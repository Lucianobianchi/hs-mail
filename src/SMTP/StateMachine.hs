module SMTP.StateMachine 
(processCmd) where

import System.IO
import Control.Monad.State
import Types
import MailStore
import NetworkUtils(send, isValidEmail)
import Network.Socket

import Data.Time.Clock

saveMailInSession :: SessionState -> IO ()
saveMailInSession s = do
  now <- getCurrentTime
  saveMail Mail{from=mailFrom s, to=mailRcpt s, content=mailData s, sentTime=now}

-- TODO: se me está repitiendo código en esto pero es la forma mas concisa que me anduvo.
-- Ver como se puede mejorar. Y encima me tuve que implementar el when no se por que.
whenS cond m1 m2 = 
  if cond
  then
    m1
  else
    m2

noSuchUserError socket session arg = do
  send socket ("550 no such user - " ++ arg)
  return session
    
processCmd :: Socket -> FSM SessionState SMTPCommand
processCmd socket session cmd = 
  let (cmdName, arg) = cmd 
  in
  case (step session, cmdName) of 
    (StandBy, Helo) -> do
      send socket "250 Hello, please to meet you"
      return SessionState{step=Helo, mailRcpt=[], mailFrom="", mailData=""}
  
    (Helo, MailFrom) -> do
      whenS (not (isValidEmail arg)) (noSuchUserError socket session arg) $ do
        send socket "250 Ok"
        return SessionState{step=MailFrom, mailRcpt=[], mailFrom=arg, mailData=""}

    (MailFrom, MailRcpt) -> do
      whenS (not (isValidEmail arg)) (noSuchUserError socket session arg) $ do
        send socket "250 Ok"
        return SessionState{step=MailRcpt, mailRcpt=[arg], mailFrom=mailFrom session, mailData=""}

    (MailRcpt, MailRcpt) -> do
      whenS (not (isValidEmail arg)) (noSuchUserError socket session arg) $ do
        send socket "250 Ok"
        return SessionState{step=MailRcpt, mailRcpt=(mailRcpt session) ++ [arg], mailFrom=mailFrom session, mailData=""}
  
    (MailRcpt, DataStart) -> do
      send socket "354 End data with <CR><LF>.<CR><LF>"
      return SessionState{step=DataStart, mailRcpt=mailRcpt session, mailFrom=mailFrom session, mailData=""}
  
    (DataStart, DataLine) -> do
      return SessionState{step=DataLine, mailRcpt=mailRcpt session, mailFrom=mailFrom session, mailData=arg}
    
    (DataLine, DataLine) -> do
      return SessionState{step=DataLine, mailRcpt=mailRcpt session, mailFrom=mailFrom session, mailData=(mailData session) ++ "\n" ++ arg}
    
    (DataLine, StandBy) -> do
      send socket "250 Ok queued"
      saveMailInSession session
      return SessionState{step=StandBy, mailRcpt=[], mailFrom="", mailData=""}
    
    (StandBy, Exit) -> do
      send socket "Bye!"
      return SessionState{step=Exit, mailRcpt=[], mailFrom="", mailData=""}
    
    (_, _) -> do
      send socket "503 Bad sequence of commands"
      return session