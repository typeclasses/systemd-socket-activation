module SocketActivation.Concepts
    (
        Recipient (..),
        ProcessID,
        Count (..),
        Name (..),
        Names (..),
        VarName (..),
        Fd (..),
        Socket,
        Error (..),
    )
    where

import Essentials

import Control.Exception (Exception (..), SomeException (..))
import Data.String (IsString, String)
import Data.Text (Text)
import Data.Typeable (cast)
import Network.Socket (Socket)
import Numeric.Natural (Natural)
import Prelude (show)
import System.Posix.Types (Fd (..), ProcessID)

import qualified Data.Text as Text

{-| The ID of the process to whom systemd has given the sockets

A process should not use sockets that are intended for someone else, so we
should always check that this matches our own PID before proceeding doing
anything with the sockets. -}
newtype Recipient = RecipientPID { recipientPID :: ProcessID }
    deriving stock (Eq, Show)

{-| The number of sockets that systemd has given the process -}
newtype Count = CountNat { countNat :: Natural }
    deriving stock (Eq, Show)

{-| The name of a socket, corresponding to the socket's FileDescriptorName in
    the systemd config -}
newtype Name = NameText { nameText :: Text }
    deriving stock (Eq, Ord, Show)
    deriving newtype IsString

{-| The names of the sockets that we have been given, corresponding to the
    FileDescriptorName of each systemd socket -}
newtype Names = NamesList { namesList :: [Name] }
    deriving stock (Eq, Show)

data VarName = LISTEN_PID | LISTEN_FDS | LISTEN_FDNAMES
    deriving stock (Eq, Show, Enum, Bounded)

data Error =
    Missing VarName
  | Invalid VarName
  | WrongProcess
  | NoSuchName Name [Name]
    deriving stock Show

instance Exception Error where
    fromException (SomeException e) = cast e
    toException = SomeException
    displayException = \case
        Missing v -> unwords
            [ "The environment variable"
            , tshow @VarName v
            , "is required but not present."
            ]
        Invalid v -> unwords
            [ "The environment variable"
            , tshow @VarName v
            , "has a malformed value that could not be parsed."
            ]
        WrongProcess -> unwords
            [ "A socket is present, but it was rejected because"
            , tshow @VarName LISTEN_PID
            , "differs from the current process ID."
            ]
        NoSuchName wanted found -> unwords
            [ "Cannot find a socket named"
            , quote (nameText wanted) <> "."
            , case found of
                [] -> "There are no available sockets."
                [x] -> Text.unwords
                    [ "This is one available socket and its name is"
                    , quote (nameText x) <> "."
                    ]
                xs -> Text.unwords
                    [ "The available sockets are:"
                    , Text.intercalate ", " (nameText <$> xs) <> "."
                    ]
            ]

quote :: Text -> Text
quote x = "‘" <> x <> "’"

tshow :: forall a. Show a => a -> Text
tshow = Text.pack . show

unwords :: [Text] -> String
unwords = Text.unpack . Text.unwords
