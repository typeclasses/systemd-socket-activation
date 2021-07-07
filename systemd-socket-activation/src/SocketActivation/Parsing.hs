module SocketActivation.Parsing where

import Relude

import Foreign.C.Types (CInt)

import qualified Data.Text as Text

import SocketActivation.Concepts

readRecipient :: Text -> Maybe Recipient
readRecipient = read >=> wrap
  where
    read = readMaybe @ProcessID . Text.unpack
    wrap = return . RecipientPID

readCount :: Text -> Maybe Count
readCount = read >=> convert >=> wrap
  where
    read = readMaybe @CInt . Text.unpack
    convert = toIntegralSized :: CInt -> Maybe Natural
    wrap = return . CountNat

readNames :: Text -> Names
readNames = NamesList . fmap NameText . Text.splitOn ":"
