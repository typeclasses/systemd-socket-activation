module SocketActivation.IO where

import Relude

import SocketActivation.Concepts

newtype IO' a = IO' { run :: IO (Either Error a) }
    deriving (Functor, Applicative, Monad, MonadIO) via ExceptT Error IO

throwError :: Error -> IO' a
throwError = IO' . return . Left
