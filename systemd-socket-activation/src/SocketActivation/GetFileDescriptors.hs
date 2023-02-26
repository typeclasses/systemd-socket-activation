module SocketActivation.GetFileDescriptors where

import Essentials

import Data.Either (Either)
import Data.List (take)
import Prelude (fromIntegral)
import System.IO (IO)

import SocketActivation.Concepts (Fd (..), Error, Count (countNat))
import SocketActivation.Env (getEnv')
import SocketActivation.IO (IO' (IO', run))

{-| Get a list of file descriptors for the sockets -}
getFileDescriptorList :: IO (Either Error [Fd])
getFileDescriptorList = run $ IO' (getEnv' @Count) <&> fds

fds :: Count -> [Fd]
fds n = take (fromIntegral (countNat n)) [Fd 3 ..]
