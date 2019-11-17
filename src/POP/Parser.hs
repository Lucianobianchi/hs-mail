module POP.Parser  
( popLineParser ) where 
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

popLineParser :: SessionState -> Parser SMTPCommand
popLineParser session = 
  case (step session) of 
    StandBy -> cmdLineParser "HELO" <|> quitParser
    Helo -> cmdLineParser "MAIL FROM:"
    MailFrom -> cmdLineParser "RCPT TO:"
    MailRcpt -> cmdLineParser "DATA"
    DataStart -> do
      argument <- manyTill anyChar newline
      return (DataLine, argument)
    DataLine -> do
      argument <- (string "\n\r") <|> (manyTill anyChar newline) 
      if argument == "\n\r" 
        then return (StandBy, argument)
        else return (DataLine, argument)

