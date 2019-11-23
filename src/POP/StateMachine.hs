module POP.StateMachine 
(processCmd) where

import System.IO
import Control.Monad.State
import Types
import POP.Types
import MailStore
import NetworkUtils(send)
import Network.Socket

-- getStats :: String -> String
-- getStats

processCmd :: Socket -> FSM POPSessionState POPCommand
processCmd socket session cmd = 
  let (cmdName, arg) = cmd 
  in
  case (step session, cmdName) of 
    (StandBy, User) -> do
      send socket "OK"
      return POPSessionState{step=User, user=arg, pass=""}
    
    (User, Pass) -> do
      send socket "OK"
      return POPSessionState{step=LoggedIn, user=user session, pass=arg}
    
    (LoggedIn, Stat) -> do
      send socket "stats!"
      return session
    
    (LoggedIn, Retr) -> do
      send socket "retr!"
      return session
    
    (LoggedIn, Dele) -> do
      send socket "dele!"
      return session
    
    (_, Exit) -> do
      send socket "bye!"
      return POPSessionState{step=Exit, user="", pass=""}
    
    
      
      
      