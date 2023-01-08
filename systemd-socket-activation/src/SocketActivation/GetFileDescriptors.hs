module SocketActivation.GetFileDescriptors where

import Control.Monad (Monad (return, (>>=)))
import Data.Either (Either)
import Data.Function ((.))
import Data.Int (Int)
import Data.List (take)
import Numeric.Natural (Natural)
import Prelude (fromIntegral)
import System.IO (IO)

import SocketActivation.Concepts (Fd (..), Error, Count (countNat))
import SocketActivation.Env (getEnv')
import SocketActivation.IO (IO' (IO', run))

{-| Get a list of file descriptors for the sockets -}
getFileDescriptorList :: IO (Either Error [Fd])
getFileDescriptorList = run (getCount >>= enumerateFds)
  where
    getCount = IO' (getEnv' @Count)
    enumerateFds = return . fds

fds :: Count -> [Fd]
fds n = take (convert n) [firstFd ..]
  where
    convert = (fromIntegral :: Natural -> Int) . countNat
    firstFd = Fd 3
