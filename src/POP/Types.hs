module POP.Types 
(
  POPSessionState(POPSessionState, step, user, pass),
  POPStep(User, Pass, LoggedIn, List, Stat, Retr, Dele, StandBy, Exit),
  POPCommand
) where 

-- TODO: diferenciar entre step y command. No son lo mismo y se ve mas claro en POP
data POPStep = User | Pass | LoggedIn | List | Stat | Retr | Dele | StandBy | Exit deriving (Show, Read, Eq)

type POPCommand = (POPStep, String)

data POPSessionState = POPSessionState { 
  user :: String, 
  pass :: String,
  step :: POPStep
} deriving (Show, Read, Eq)