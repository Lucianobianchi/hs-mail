module POP.StateMachine 
(processCmd) where

import System.IO
import Control.Monad.State
import Types
import POP.Types
import POP.POPUtils(getStat, getRetr, getList)
import MailStore
import NetworkUtils(send)
import Network.Socket

processCmd :: Socket -> FSM POPSessionState POPCommand
processCmd socket session cmd = 
  let (stepName, arg) = cmd 
  in
  case (step session, stepName) of 
    (StandBy, User) -> do
      if (length arg) == 0
      then
        do 
          send socket "-ERR Please specify a user"
          return session
      else
        do 
          send socket "+OK"
          return POPSessionState{step=User, user=arg, pass=""}
    
    (User, Pass) -> do
      -- Any password is valid right now
      send socket "+OK"
      return POPSessionState{step=LoggedIn, user=user session, pass=arg}
    
      -- TODO: estoy repitiendo codigo fuerte aca
    (LoggedIn, Stat) -> do
      getMails (user session) (\mails -> send socket $ "+OK " ++ (getStat mails))
      return session
    
    (LoggedIn, Retr) -> do
      let index = read arg
      getMails (user session) (\mails -> do
        if (length mails) >= index -- index starts at 1 in POP
        then
          do
            send socket $ "+OK " ++ (getRetr mails index)
            return session
        else
          do
            send socket $ "-ERR no message at index " ++ (show index)
            return session)

    (LoggedIn, List) -> do
      getMails (user session) (\mails -> send socket $ "+OK\n" ++ (getList mails))
      return session
    
    (LoggedIn, Dele) -> do
      let index = read arg
      deleteMail (user session) (index-1)
      send socket $ "+OK deleted " ++ (show index)
      return session

    (_, Reset) -> do
      send socket "+OK reset session"
      return POPSessionState{step=StandBy, user="", pass=""}

    (_, Exit) -> do
      send socket "+OK bye!"
      return POPSessionState{step=Exit, user="", pass=""}

    (_, _) -> do
      send socket "-ERR You must log in first!"
      return session
      
      -- TODO: devolver 0 cuando no hay ningun mail en STAT y LIST
      -- TODO: puedo poner el send socket OK en una funcion en la seccion where del case