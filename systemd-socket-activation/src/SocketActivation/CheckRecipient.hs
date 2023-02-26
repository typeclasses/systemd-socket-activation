-- | Verification of the socket recipient PID

module SocketActivation.CheckRecipient where

import Essentials

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (Either)
import System.IO (IO)

import qualified Control.Monad as Monad
import qualified System.Posix.Process as Sys

import SocketActivation.Concepts (Error(WrongProcess), Recipient (recipientPID))
import SocketActivation.Env (getEnv')
import SocketActivation.IO (IO' (IO', run), throwError)

checkRecipient :: IO (Either Error ())
checkRecipient = run do
    recipient <- IO' $ getEnv' @Recipient
    IO' $ checkRecipient' recipient

checkRecipient' :: Recipient -> IO (Either Error ())
checkRecipient' recipient = run do
    myPid <- liftIO Sys.getProcessID
    Monad.unless (recipientPID recipient == myPid) $ throwError WrongProcess
