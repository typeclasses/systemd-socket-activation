module SocketActivation.Concepts
  ( Recipient (..)
  , ProcessID
  , Count (..)
  , Name (..)
  , Names (..)
  , VarName (..)
  , Fd (..)
  , Socket
  , Error (..)
  ) where

import Relude

import Network.Socket (Socket)
import System.Posix.Types (Fd (..), ProcessID)

-- | The ID of the process to whom systemd has given the sockets. A process should not use sockets that are intended for someone else, so we should always check that this matches our own PID before proceeding doing anything with the sockets.
newtype Recipient = RecipientPID { recipientPID :: ProcessID }
    deriving stock (Eq, Show)

-- | The number of sockets that systemd has given the process.
newtype Count = CountNat { countNat :: Natural }
    deriving stock (Eq, Show)

-- | The name of a socket, corresponding to the socket's FileDescriptorName in the systemd config.
newtype Name = NameText { nameText :: Text }
    deriving stock (Eq, Ord, Show)
    deriving newtype IsString

-- | The names of the sockets that we have been given, corresponding to the FileDescriptorName of each systemd socket.
newtype Names = NamesList { namesList :: [Name] }
    deriving stock (Eq, Show)

data VarName = LISTEN_PID | LISTEN_FDS | LISTEN_FDNAMES
    deriving stock (Eq, Show, Enum, Bounded)

data Error = Missing VarName | Invalid VarName | WrongProcess | NoSuchName Name
    deriving stock Show
    deriving anyclass Exception
