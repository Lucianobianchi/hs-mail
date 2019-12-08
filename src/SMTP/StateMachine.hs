module SMTP.StateMachine 
(processCmd) where

import System.IO
import Control.Monad.State
import Types
import SMTP.Types
import MailStore
import Utils(send, isValidEmail)
import Network.Socket

import Data.Time.Clock

saveMailInSession :: SMTPSessionState -> IO ()
saveMailInSession s = do
  now <- getCurrentTime
  saveMail Mail{from=mailFrom s, to=mailRcpt s, content=mailData s, sentTime=now}

whenS cond m1 m2 = 
  if cond
  then
    m1
  else
    m2

noSuchUserError socket session arg = do
  send socket ("550 no such user - " ++ arg)
  return session
    
processCmd :: Socket -> FSM SMTPSessionState SMTPCommand
processCmd socket session cmd = 
  let (nextStep, arg) = cmd 
  in
  case (step session, nextStep) of 
    (StandBy, Helo) -> do
      send socket "250 Hello, please to meet you"
      return session {step=Helo}
  
    (Helo, MailFrom) -> do
      whenS (not $ isValidEmail arg) (noSuchUserError socket session arg) $ do
        send socket "250 Ok"
        return session {step=MailFrom, mailFrom=arg}

    (MailFrom, MailRcpt) -> do
      whenS (not $ isValidEmail arg) (noSuchUserError socket session arg) $ do
        send socket "250 Ok"
        return session {step=MailRcpt, mailRcpt=[arg]}

    (MailRcpt, MailRcpt) -> do
      whenS (not $ isValidEmail arg) (noSuchUserError socket session arg) $ do
        send socket "250 Ok"
        return session {step=MailRcpt, mailRcpt=(mailRcpt session) ++ [arg]}
  
    (MailRcpt, DataStart) -> do
      send socket "354 End data with <CR><LF>.<CR><LF>"
      return session {step=DataStart}
  
    (DataStart, DataLine) -> do
      return session {step=DataLine, mailData=arg}
    
    (DataLine, DataLine) -> do
      return session {mailData=(mailData session) ++ "\n" ++ arg}
    
    (DataLine, DataEnd) -> do
      send socket "250 Ok queued"
      saveMailInSession session
      return session {step=Helo, mailRcpt=[], mailFrom="", mailData=""}
    
    (_, Exit) -> do
      send socket "Bye!"
      return session {step=Exit, mailRcpt=[], mailFrom="", mailData=""}
    
    (_, _) -> do
      send socket "503 Bad sequence of commands"
      return session