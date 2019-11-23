module SMTP.Parser  
( smtpLineParser ) where 
import Types
import Text.ParserCombinators.Parsec
import Data.Char

cmdLine :: String -> SMTPStep -> Parser SMTPCommand
cmdLine cmdStr smtpStep = do
  cmd <- string cmdStr
  many (char ' ')
  argument <- manyTill anyChar newline
  return (smtpStep, argument)

addressLine:: String -> SMTPStep -> Parser SMTPCommand
addressLine cmdStr smtpStep = do
  cmd <- string cmdStr
  many (char ' ')
  argument <- (char '<') *> manyTill anyChar (char '>') <* newline
  return (smtpStep, argument)

smtpLineParser :: SessionState -> Parser SMTPCommand
smtpLineParser session = 
  case (step session) of 
    StandBy -> cmdLine "HELO" Helo <|> cmdLine "QUIT" Exit
    Helo -> addressLine "MAIL FROM:" MailFrom
    MailFrom -> mailRcpt
    MailRcpt -> mailRcpt <|> cmdLine "DATA" DataStart
    DataStart -> dataLine -- TODO: check if DATA is completely empty
    DataLine -> cmdLine "." StandBy <|> dataLine
    where
      mailRcpt = addressLine "RCPT TO:" MailRcpt
      mailFrom = addressLine "MAIL FROM:" MailFrom
      dataLine = do 
        arg <- manyTill anyChar newline
        return (DataLine, arg)

