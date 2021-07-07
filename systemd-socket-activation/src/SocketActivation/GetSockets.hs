module SocketActivation.GetSockets where

import Relude

import qualified Network.Socket as Net

import SocketActivation.Concepts
import SocketActivation.GetFileDescriptors
import SocketActivation.IO

getSocketList :: IO (Either Error [Socket])
getSocketList = run (getFds >>= convertToSockets)
  where
    getFds = IO' getFileDescriptorList
    convertToSockets = traverse (liftIO . fdSocket)

fdSocket :: Fd -> IO Socket
fdSocket (Fd i) = Net.mkSocket i
