module SMTP.Parser  
( smtpLineParser ) where 
import Types
import SMTP.Types
import Text.ParserCombinators.Parsec
import Data.Char

cmdLine :: String -> SMTPStep -> Parser SMTPCommand
cmdLine stepStr smtpStep = do
  cmd <- string stepStr
  many (char ' ')
  argument <- manyTill anyChar newline
  return (smtpStep, argument)

addressLine:: String -> SMTPStep -> Parser SMTPCommand
addressLine stepStr smtpStep = do
  cmd <- string stepStr
  many (char ' ')
  argument <- (char '<') *> manyTill anyChar (char '>') <* newline
  return (smtpStep, argument)

smtpLineParser :: SMTPSessionState -> Parser SMTPCommand
smtpLineParser session = 
  case (step session) of 
    StandBy -> cmdLine "HELO" Helo <|> exit
    Helo -> addressLine "MAIL FROM:" MailFrom <|> exit
    MailFrom -> mailRcpt <|> exit
    MailRcpt -> mailRcpt <|> cmdLine "DATA" DataStart <|> exit
    DataStart -> dataLine -- TODO: check if DATA is completely empty
    DataLine -> cmdLine "." StandBy <|> dataLine
    where
      exit = cmdLine "QUIT" Exit
      mailRcpt = addressLine "RCPT TO:" MailRcpt
      mailFrom = addressLine "MAIL FROM:" MailFrom
      dataLine = do 
        arg <- manyTill anyChar newline
        return (DataLine, arg)

