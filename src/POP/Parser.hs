module POP.Parser  
( popLineParser ) where 
import Types
import POP.Types
import Text.ParserCombinators.Parsec
import Data.Char

cmdLine :: String -> POPStep -> Parser POPCommand
cmdLine cmdStr smtpStep = do
  cmd <- string cmdStr
  many (char ' ')
  argument <- manyTill anyChar newline
  return (smtpStep, argument)

popLineParser :: POPSessionState -> Parser POPCommand
popLineParser session = 
  case (step session) of 
    StandBy -> cmdLine "user" User <|> cmdLine "quit" Exit
    User -> cmdLine "pass" Pass <|> cmdLine "quit" Exit
    LoggedIn -> cmdLine "stat" Stat <|> cmdLine "list" List <|> cmdLine "retr" Retr <|> cmdLine "dele" Dele <|> cmdLine "quit" Exit

