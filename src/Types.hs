module Types  
( SMTPStep(StandBy, Helo, MailFrom, MailRcpt, DataStart, DataLine, Exit), 
  SMTPCommand,
  FSM,
  SessionState(SessionState, step, mailRcpt, mailFrom, mailData),
  Mail(Mail, from, to, content, sentTime)
) where 

data SMTPStep = Helo | MailFrom | MailRcpt | DataStart | DataLine | StandBy | Exit deriving (Show, Eq)

type SMTPCommand = (SMTPStep, String)

data SessionState = SessionState { 
  mailFrom :: String, 
  mailRcpt :: String,
  mailData :: String,
  step :: SMTPStep
} deriving (Show, Read, Eq)

data Mail = Mail {
  from ::  String,
  to :: String,
  -- TODO
  -- cc?
  -- cco?
  content :: String,
  sentTime :: Int
} deriving (Show, Read)

-- Functional State Machine
-- IO lets us have side effects (actions)
type FSM s e = s -> e -> IO s