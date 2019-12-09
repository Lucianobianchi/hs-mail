module POP.Parser  
( popLineParser ) where 
import Types
import POP.Types
import Text.ParserCombinators.Parsec
import Text.Parsec.Rfc2234(caseString)
import Data.Char

cmdLine :: String -> POPStep -> Parser POPCommand
cmdLine cmdStr popStep = do
  cmd <- caseString cmdStr
  many (char ' ')
  manyTill anyChar newline
  return (popStep, argument)

cmdLineArg :: String -> POPStep -> Parser POPCommand
cmdLineArg cmdStr popStep = do
  cmd <- caseString cmdStr
  many1 (char ' ')
  argument <- manyTill anyChar newline
  return (popStep, argument)

cmdLineIntArg :: String -> POPStep -> Parser POPCommand
cmdLineIntArg cmdStr popStep = do
  cmd <- caseString cmdStr
  many (char ' ')
  argument <- many1 digit <* newline
  return (popStep, argument)

popLineParser :: POPSessionState -> Parser POPCommand
popLineParser session = 
  case (step session) of 
    StandBy -> cmdLineArg "user" User <|> quitParser
    User -> cmdLineArg "pass" Pass <|> quitParser
    LoggedIn -> cmdLine "stat" Stat <|> cmdLine "list" List <|>
                cmdLineIntArg "retr" Retr <|> 
                cmdLineIntArg "dele" Dele <|>  quitParser 
    where 
      quitParser = cmdLine "quit" Exit


