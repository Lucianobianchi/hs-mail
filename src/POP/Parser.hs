module POP.Parser  
( popLineParser ) where 
import Types
import POP.Types
import Text.ParserCombinators.Parsec
import Data.Char

cmdLine :: String -> POPStep -> Parser POPCommand
cmdLine cmdStr popStep = do
  cmd <- string cmdStr
  many (char ' ')
  argument <- manyTill anyChar newline
  return (popStep, argument)

cmdLineIntArg :: String -> POPStep -> Parser POPCommand
cmdLineIntArg cmdStr popStep = do
  cmd <- string cmdStr
  many (char ' ')
  argument <- manyTill digit newline
  return (popStep, argument)

popLineParser :: POPSessionState -> Parser POPCommand
popLineParser session = 
  case (step session) of 
    StandBy -> cmdLine "user" User <|> quitOrReset
    User -> cmdLine "pass" Pass <|> quitOrReset
    LoggedIn -> cmdLine "stat" Stat <|> cmdLine "list" List <|>  quitOrReset <|>
                cmdLineIntArg "retr" Retr <|> 
                cmdLineIntArg "dele" Dele
    where 
      resetParser = cmdLine "rset" Reset
      quitParser = cmdLine "quit" Exit
      quitOrReset = quitParser <|> resetParser


