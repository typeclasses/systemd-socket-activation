module SocketActivation.Env where

import Relude

import qualified Data.Text as Text

import qualified System.Environment as Sys

import SocketActivation.Concepts
import SocketActivation.IO
import SocketActivation.Parsing

getVarText :: VarName -> IO (Either Error Text)
getVarText name = run (getMaybe >>= throwIfMissing >>= pack)
  where
    throwIfMissing = maybe (throwError (Missing name)) return
    getMaybe = liftIO $ Sys.lookupEnv $ show @String @VarName name
    pack = return . Text.pack

getEnvVars :: IO [(VarName, Maybe Text)]
getEnvVars = traverse (\x -> getVarText x >>= \y -> return (x, either (\_ -> Nothing) Just y)) [minBound .. maxBound]

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
