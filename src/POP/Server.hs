module POP.Server 
(popServer) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Data.Either
import Control.Monad.State
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.UTF8 (toString)

import POP.Parser
import POP.StateMachine
import Types
import NetworkUtils(send, sendMany)
  
popServer socket = do
  print "Starting server..."
  execStateT (popProcessor socket) SessionState{mailFrom="", mailRcpt="", mailData="", step=StandBy}

popProcessor :: Socket -> StateT SessionState IO ()
popProcessor socket = do 
  msg <- lift $ recv socket 1024

  unless (S.null msg) $ do
    serverState <- get
    let message = toString msg

    let parseResult = parse (popLineParser serverState) "" message

    case parseResult of 
      Left err -> do
        lift $ print err
        -- TODO: hacer esto bien y responder con los códigos de SMTP
        let errorResponse = map messageString (errorMessages err)
        lift $ sendMany socket errorResponse
        popProcessor socket

      Right cmd -> do
        nextState <- lift $ processCmd socket serverState cmd 
        lift $ print nextState
        put nextState
        unless (Exit == step nextState) (popProcessor socket)