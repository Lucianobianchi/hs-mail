module POP.Types 
(
  POPSessionState(POPSessionState, step, user, pass),
  POPStep(User, Pass, LoggedIn, List, Stat, Retr, Dele, StandBy, Exit, Reset),
  POPCommand
) where 

data POPStep = User | Pass | LoggedIn | List | Stat | Retr | Dele | StandBy | Exit | Reset deriving (Show, Read, Eq)

type POPCommand = (POPStep, String)

data POPSessionState = POPSessionState { 
  user :: String, 
  pass :: String,
  step :: POPStep
} deriving (Show, Read, Eq)