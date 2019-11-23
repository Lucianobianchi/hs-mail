module Types  
(
  FSM,
  Mail(Mail, from, to, content, sentTime)
) where 

import Data.Time


data Mail = Mail {
  from ::  String,
  to :: [String],
  content :: String,
  sentTime :: UTCTime
} deriving (Show, Read)

-- Functional State Machine
-- IO lets us have side effects (actions)
type FSM s e = s -> e -> IO s