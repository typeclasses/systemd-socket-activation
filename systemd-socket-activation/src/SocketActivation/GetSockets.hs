module SocketActivation.GetSockets where

import           Control.Monad                       (Monad ((>>=)))
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Either                         (Either)
import           Data.Function                       ((.))
import           Data.Traversable                    (Traversable (traverse))
import           System.IO                           (IO)

import qualified Network.Socket                      as Net

import           SocketActivation.Concepts
import           SocketActivation.GetFileDescriptors
import           SocketActivation.IO

getSocketList :: IO (Either Error [Socket])
getSocketList = run (getFds >>= convertToSockets)
  where
    getFds = IO' getFileDescriptorList
    convertToSockets = traverse (liftIO . fdSocket)

fdSocket :: Fd -> IO Socket
fdSocket (Fd i) = Net.mkSocket i
