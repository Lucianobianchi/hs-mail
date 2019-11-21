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
import NetworkUtils(send, sendMany)
  
smtpServer socket = do
  print "Starting server..."
  execStateT (smtpProcessor socket) SessionState{mailFrom="", mailRcpt="", mailData="", step=StandBy}

smtpProcessor :: Socket -> StateT SessionState IO ()
smtpProcessor socket = do 
  msg <- lift $ recv socket 1024

  unless (S.null msg) $ do
    serverState <- get
    let message = toString msg

    let parseResult = parse (smtpLineParser serverState) "" message

    case parseResult of 
      Left err -> do
        lift $ print err
        -- TODO: hacer esto bien y responder con los códigos de SMTP
        let errorResponse = map messageString (errorMessages err)
        lift $ sendMany socket errorResponse
        smtpProcessor socket

      Right cmd -> do
        nextState <- lift $ processCmd socket serverState cmd 
        lift $ print nextState
        put nextState
        unless (Exit == step nextState) (smtpProcessor socket)