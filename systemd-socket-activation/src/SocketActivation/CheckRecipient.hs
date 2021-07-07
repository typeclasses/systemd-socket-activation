-- | Verification of the socket recipient PID

module SocketActivation.CheckRecipient where

import Relude

import qualified System.Posix.Process as Sys

import SocketActivation.Concepts
import SocketActivation.Env
import SocketActivation.IO

checkRecipient :: IO (Either Error ())
checkRecipient = run (getIt >>= checkIt)
  where
    getIt = IO' (getEnv' @Recipient)
    checkIt = IO' . checkRecipient'

checkRecipient' :: Recipient -> IO (Either Error ())
checkRecipient' x = run (getMyPid >>= throwIfDifferent)
  where
    getMyPid = liftIO Sys.getProcessID
    throwIfDifferent y = if recipientPID x == y then return () else throwError WrongProcess
