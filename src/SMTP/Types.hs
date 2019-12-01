module SMTP.Types  
( SMTPStep(StandBy, Helo, MailFrom, MailRcpt, DataStart, DataLine, DataEnd, Exit), 
  SMTPCommand,
  SMTPSessionState(SMTPSessionState, step, mailRcpt, mailFrom, mailData),
) where 

data SMTPStep = Helo | MailFrom | MailRcpt | DataStart | DataLine | DataEnd | StandBy | Exit deriving (Show, Read, Eq)

data SMTPSessionState = SMTPSessionState { 
  mailFrom :: String, 
  mailRcpt :: [String],
  mailData :: String,
  step :: SMTPStep
} deriving (Show, Read, Eq)

type SMTPCommand = (SMTPStep, String)