module SocketActivation.GetFileDescriptors where

import Relude

import SocketActivation.Concepts
import SocketActivation.Env
import SocketActivation.IO

-- | Get a list of file descriptors for the sockets.
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
