module SocketActivation.GetSockets where

import Essentials

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (Either)
import System.IO (IO)

import qualified Network.Socket as Net

import SocketActivation.Concepts (Socket, Fd (..), Error)
import SocketActivation.GetFileDescriptors (getFileDescriptorList)
import SocketActivation.IO (IO' (IO', run))

getSocketList :: IO (Either Error [Socket])
getSocketList = run $
    IO' getFileDescriptorList >>= traverse (liftIO . fdSocket)

fdSocket :: Fd -> IO Socket
fdSocket (Fd i) = Net.mkSocket i
