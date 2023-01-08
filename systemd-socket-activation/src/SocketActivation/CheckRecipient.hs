-- | Verification of the socket recipient PID

module SocketActivation.CheckRecipient where

import Control.Monad (Monad (return, (>>=)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (Either)
import Data.Eq (Eq ((==)))
import Data.Function ((.))
import System.IO (IO)

import qualified System.Posix.Process as Sys

import SocketActivation.Concepts (Error(WrongProcess), Recipient (recipientPID))
import SocketActivation.Env (getEnv')
import SocketActivation.IO (IO' (IO', run), throwError)

checkRecipient :: IO (Either Error ())
checkRecipient = run (getIt >>= checkIt)
  where
    getIt = IO' (getEnv' @Recipient)
    checkIt = IO' . checkRecipient'

checkRecipient' :: Recipient -> IO (Either Error ())
checkRecipient' x = run (getMyPid >>= throwIfDifferent)
  where
    getMyPid = liftIO Sys.getProcessID
    throwIfDifferent y =
        if recipientPID x == y then return () else throwError WrongProcess
