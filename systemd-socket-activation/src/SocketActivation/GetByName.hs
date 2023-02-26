module SocketActivation.GetByName where

import Essentials

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (Either)
import Data.List (zip)
import Data.Map (Map)
import System.IO (IO)

import qualified Data.Map as Map

import SocketActivation.Concepts
    (Socket, Fd, Error (NoSuchName), Names (namesList), Name)
import SocketActivation.Env (getEnv')
import SocketActivation.GetFileDescriptors (getFileDescriptorList)
import SocketActivation.GetSockets (getSocketList, fdSocket)
import SocketActivation.IO (IO' (IO', run), throwError)

getNameList :: IO (Either Error [Name])
getNameList = run $ IO' (getEnv' @Names) <&> namesList

getFileDescriptorMap :: IO (Either Error (Map Name Fd))
getFileDescriptorMap = run $ entries <&> Map.fromList
  where
    entries = zip <$> IO' getNameList <*> IO' getFileDescriptorList

getSocketMap :: IO (Either Error (Map Name Socket))
getSocketMap = run (entries <&> Map.fromList)
  where
    entries = zip <$> IO' getNameList <*> IO' getSocketList

getSocketByName :: Name -> IO (Either Error Socket)
getSocketByName name = run do
    m <- IO' getFileDescriptorMap
    fd <- case Map.lookup name m of
        Nothing -> throwError (NoSuchName name (Map.keys m))
        Just x -> pure x
    liftIO $ fdSocket fd
