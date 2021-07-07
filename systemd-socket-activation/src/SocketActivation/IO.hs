module SocketActivation.IO where

import           Control.Applicative        (Applicative)
import           Control.Monad              (Functor, Monad (return))
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Either                (Either (Left))
import           Data.Function              ((.))
import           System.IO                  (IO)

import           SocketActivation.Concepts

newtype IO' a = IO' { run :: IO (Either Error a) }
    deriving (Functor, Applicative, Monad, MonadIO) via ExceptT Error IO

throwError :: Error -> IO' a
throwError = IO' . return . Left
