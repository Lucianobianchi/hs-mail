module POP.StateMachine 
(processCmd) where

import System.IO
import Control.Monad.State
import Control.Monad (unless)
import Types
import POP.Types
import POP.POPUtils(getStat, getRetr, getList)
import MailStore
import Utils(send)
import Network.Socket
import Text.Read(readMaybe)

processCmd :: Socket -> FSM POPSessionState POPCommand
processCmd socket session cmd = 
  let (stepName, arg) = cmd 
  in
  case (step session, stepName) of 
    (StandBy, User) -> do
      if (length arg) == 0
      then
        do 
          sendError socket "-ERR Please specify a user"
          return session
      else
        do 
          sendOk socket ""
          return session {step=User, user=arg}
    
    (User, Pass) -> do
      -- Any password is valid right now
      sendOk socket ""
      return session{step=LoggedIn, pass=arg}
    
    (LoggedIn, Stat) -> do
      getMails (user session) (\mails -> sendOk socket (getStat mails))
      return session
    
    (LoggedIn, Retr) -> do
      let index = read arg
      getMails (user session) (\mails -> do
        if index > 0 && (length mails) >= index -- index starts at 1 in POP
        then
          do
            sendOk socket (getRetr mails index)
            return session
        else
          do
            sendError socket $ "no message at index " ++ (show index)
            return session)

    (LoggedIn, List) -> do
      getMails (user session) $ \mails -> do
        sendOk socket (getStat mails)
        unless (length mails == 0) (send socket $ getList mails)
      return session
    
    (LoggedIn, Dele) -> do
      let index = read arg
      deleteMail (user session) (index - 1)
      sendOk socket $ "deleted " ++ (show index)
      return session

    (_, Reset) -> do
      sendOk socket "reset session"
      return session {step=StandBy, user="", pass=""}

    (_, Exit) -> do
      sendOk socket "bye!"
      return session {step=Exit}

    (_, _) -> do
      sendError socket "You must log in first!"
      return session

    where
      sendError socket message = send socket ("-ERR " ++ message)
      sendOk socket "" = send socket "+OK"
      sendOk socket message = send socket ("+OK " ++ message)