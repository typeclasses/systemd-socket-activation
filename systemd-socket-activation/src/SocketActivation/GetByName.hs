module SocketActivation.GetByName where

import           Control.Applicative                 (Applicative ((<*>)),
                                                      (<$>))
import           Control.Monad                       (Monad (return, (>>=)))
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Either                         (Either)
import           Data.Function                       ((.))
import           Data.List                           (zip)
import           Data.Map                            (Map)
import           Data.Maybe                          (maybe)
import           System.IO                           (IO)

import qualified Data.Map                            as Map

import           SocketActivation.Concepts
import           SocketActivation.Env
import           SocketActivation.GetFileDescriptors
import           SocketActivation.GetSockets
import           SocketActivation.IO

getNameList :: IO (Either Error [Name])
getNameList = run (getNames >>= unwrap)
  where
    getNames = IO' (getEnv' @Names)
    unwrap = return . namesList

getFileDescriptorMap :: IO (Either Error (Map Name Fd))
getFileDescriptorMap = run (entries >>= toMap)
  where
    entries = zip <$> keys <*> values
    keys = IO' getNameList
    values = IO' getFileDescriptorList
    toMap = return . Map.fromList

getSocketMap :: IO (Either Error (Map Name Socket))
getSocketMap = run (entries >>= toMap)
  where
    entries = zip <$> keys <*> values
    keys = IO' getNameList
    values = IO' getSocketList
    toMap = return . Map.fromList

getSocketByName :: Name -> IO (Either Error Socket)
getSocketByName name = run (getMap >>= findFd >>= convertToSocket)
  where
    getMap = IO' getFileDescriptorMap
    findFd = maybe (throwError (NoSuchName name)) return . Map.lookup name
    convertToSocket = liftIO . fdSocket
