module SocketActivation.Env where

import Control.Monad (Monad (return, (>>=)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (Either, either)
import Data.Function (($), (.))
import Data.Maybe (Maybe (..), maybe)
import Data.Text (Text)
import Data.Traversable (Traversable (traverse))
import Prelude (Bounded (maxBound, minBound))
import System.IO (IO)
import Text.Show (show)

import qualified Data.Text as Text
import qualified System.Environment as Sys

import SocketActivation.Concepts
    (Error (Invalid, Missing), VarName (..), Names, Count, Recipient)
import SocketActivation.IO (IO' (IO', run), throwError)
import SocketActivation.Parsing (readRecipient, readCount, readNames)

getVarText :: VarName -> IO (Either Error Text)
getVarText name = run (getMaybe >>= throwIfMissing >>= pack)
  where
    throwIfMissing = maybe (throwError (Missing name)) return
    getMaybe = liftIO $ Sys.lookupEnv $ show @VarName name
    pack = return . Text.pack

getEnvVars :: IO [(VarName, Maybe Text)]
getEnvVars =
    traverse
        (\x -> getVarText x >>= \y ->
            return (x, either (\_ -> Nothing) Just y))
        [minBound .. maxBound]

data Env a = Env VarName (Text -> Maybe a)

getEnv :: Env a -> IO (Either Error a)
getEnv (Env name read) = run (getText >>= readOrThrow)
  where
    getText = IO' (getVarText name)
    readOrThrow = maybe (throwError (Invalid name)) return . read

getEnv' :: Env' a => IO (Either Error a)
getEnv' = getEnv env'

class Env' a where env' :: Env a
instance Env' Recipient where env' = Env LISTEN_PID readRecipient
instance Env' Count where env' = Env LISTEN_FDS readCount
instance Env' Names where env' = Env LISTEN_FDNAMES (Just . readNames)
