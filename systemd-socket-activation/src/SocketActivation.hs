module SocketActivation
    (
    -- * Actions
      getMySocketByName

    -- * Types
    , Name (..), VarName (..), Socket, Error (..)

    ) where

import           Control.Applicative             (Applicative ((*>)))
import           Control.Monad                   (Monad (return, (>>=)))
import           Data.Either                     (Either, either)
import           System.IO                       (IO, print)

import qualified Control.Exception               as Ex

import qualified SocketActivation.CheckRecipient as SA
import qualified SocketActivation.Concepts       as SA
import qualified SocketActivation.Env            as SA
import qualified SocketActivation.GetByName      as SA

import           SocketActivation.Concepts       as Concepts

getMySocketByName :: SA.Name -> IO SA.Socket
getMySocketByName name = f SA.checkRecipient *> f (SA.getSocketByName name)
  where
    f :: IO (Either SA.Error a) -> IO a
    f = (>>= either (\e -> (SA.getEnvVars >>= print) *> Ex.throw e) return)
