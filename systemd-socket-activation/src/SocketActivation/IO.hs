module SocketActivation.IO where

import Essentials

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Either (Either (Left))
import System.IO (IO)

import SocketActivation.Concepts (Error)

newtype IO' a = IO' { run :: IO (Either Error a) }
    deriving (Functor, Applicative, Monad, MonadIO) via ExceptT Error IO

throwError :: Error -> IO' a
throwError = IO' . pure . Left
