module SMTP.Server 
(smtpServer) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Data.Either
import Control.Monad.State
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.UTF8 (toString, fromString)

import SMTP.Parser
import SMTP.StateMachine
import Types
import SMTP.Types
import NetworkUtils(send, sendMany)
  
smtpServer socket = execStateT (smtpProcessor socket) SMTPSessionState{mailFrom="", mailRcpt=[], mailData="", step=StandBy}

smtpProcessor :: Socket -> StateT SMTPSessionState IO ()
smtpProcessor socket = do 
  msg <- lift $ recv socket 1024 -- TODO: recibir y acumular hasta un \n

  unless (S.null msg) $ do
    serverState <- get
    let message = toString msg

    let parseResult = parse (smtpLineParser serverState) "" message

    case parseResult of 
      Left err -> do
        lift $ print err
        lift $ send socket $ "501 Error - unexpected command " ++ (init message)
        smtpProcessor socket

      Right cmd -> do
        nextState <- lift $ processCmd socket serverState cmd 
        lift $ print nextState
        put nextState
        unless (Exit == step nextState) (smtpProcessor socket)