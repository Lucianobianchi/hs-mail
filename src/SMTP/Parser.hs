module SMTP.Parser  
( smtpLineParser ) where 
import Types
import SMTP.Types
import Text.ParserCombinators.Parsec
import Text.Parsec.Rfc2234(caseString)
import Data.Char

cmdLine :: String -> SMTPStep -> Parser SMTPCommand
cmdLine stepStr smtpStep = do
  cmd <- caseString stepStr
  many (char ' ')
  argument <- manyTill anyChar newline
  return (smtpStep, argument)

addressLine:: String -> SMTPStep -> Parser SMTPCommand
addressLine stepStr smtpStep = do
  cmd <- caseString stepStr
  many (char ' ')
  argument <- (char '<') *> manyTill anyChar (char '>') <* newline
  return (smtpStep, argument)

smtpLineParser :: SMTPSessionState -> Parser SMTPCommand
smtpLineParser session = 
  case (step session) of 
    StandBy -> cmdLine "helo" Helo <|> exit
    Helo -> addressLine "mail from:" MailFrom <|> exit
    MailFrom -> mailRcpt <|> exit
    MailRcpt -> mailRcpt <|> cmdLine "data" DataStart <|> exit
    DataStart -> dataLine
    DataLine -> cmdLine "." DataEnd <|> dataLine
    where
      exit = cmdLine "quit" Exit
      mailRcpt = addressLine "rcpt to:" MailRcpt
      dataLine = do 
        arg <- manyTill anyChar newline
        return (DataLine, arg)

