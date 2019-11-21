module SMTP.Parser  
( smtpLineParser ) where 
import Types
import Text.ParserCombinators.Parsec
import Data.Char

toSMTPStep cmdStr =
  case cmdStr of
    "HELO" -> Helo
    "MAIL FROM:" -> MailFrom
    "RCPT TO:" -> MailRcpt
    "DATA" -> DataStart

cmdLineParser = \cmdStr -> do
  cmd <- string cmdStr
  many (char ' ')
  argument <- manyTill anyChar newline
  return (toSMTPStep cmd, argument)

quitParser = do
  string "QUIT"
  return (Exit, "")

smtpLineParser :: SessionState -> Parser SMTPCommand
smtpLineParser session = 
  case (step session) of 
    StandBy -> cmdLineParser "HELO" <|> quitParser
    Helo -> cmdLineParser "MAIL FROM:"
    MailFrom -> cmdLineParser "RCPT TO:"
    MailRcpt -> cmdLineParser "DATA"
    DataStart -> do
      argument <- manyTill anyChar newline
      return (DataLine, argument)
    DataLine -> do
      argument <- (string ".") <|> (manyTill anyChar newline) 
      if argument == "." 
        then return (StandBy, argument)
        else return (DataLine, argument)

