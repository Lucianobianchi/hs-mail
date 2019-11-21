module SMTP.StateMachine 
(processCmd) where

import System.IO
import Control.Monad.State
import Types
import MailStore
import NetworkUtils(send)
import Network.Socket

processCmd :: Socket -> FSM SessionState SMTPCommand
processCmd socket session cmd = 
  let (cmdName, arg) = cmd in
  case (step session, cmdName) of 
    (StandBy, Helo) -> do
      send socket "250 Hello, please to meet you"
      -- saveMail Mail{from="lusho", to="nicky", content="hola nicky", sentTime=1234}
      return SessionState{step=Helo, mailRcpt="", mailFrom="", mailData=""}
  
    (Helo, MailFrom) -> do 
      send socket "250 Ok"
      return SessionState{step=MailFrom, mailRcpt="", mailFrom=arg, mailData=""}
    
    (MailFrom, MailRcpt) -> do
      send socket "250 Ok"
      return SessionState{step=MailRcpt, mailRcpt=arg, mailFrom=mailFrom session, mailData=""}

    (MailRcpt, DataStart) -> do
      send socket "354 End data with <CR><LF>.<CR><LF>"
      return SessionState{step=DataStart, mailRcpt=mailRcpt session, mailFrom=mailFrom session, mailData=""}
  
    (DataStart, DataLine) -> do
      return SessionState{step=DataLine, mailRcpt=mailRcpt session, mailFrom=mailFrom session, mailData=arg}
    
    (DataLine, DataLine) -> do
      return SessionState{step=DataLine, mailRcpt=mailRcpt session, mailFrom=mailFrom session, mailData=(mailData session) ++ "\n" ++ arg}
    
    (DataLine, StandBy) -> do
      send socket "250 Ok"
      return SessionState{step=StandBy, mailRcpt="", mailFrom="", mailData=""}
    
    (StandBy, Exit) -> do
      send socket "Bye!"
      return SessionState{step=Exit, mailRcpt="", mailFrom="", mailData=""}