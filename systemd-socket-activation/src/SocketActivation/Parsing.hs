module SocketActivation.Parsing where

import Essentials

import Data.Bits (toIntegralSized)
import Data.Text (Text)
import Foreign.C.Types (CInt)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

import qualified Data.Text as Text

import SocketActivation.Concepts

readRecipient :: Text -> Maybe Recipient
readRecipient = read >=> wrap
  where
    read = readMaybe @ProcessID . Text.unpack
    wrap = pure . RecipientPID

readCount :: Text -> Maybe Count
readCount = read >=> convert >=> wrap
  where
    read = readMaybe @CInt . Text.unpack
    convert = toIntegralSized :: CInt -> Maybe Natural
    wrap = pure . CountNat

readNames :: Text -> Names
readNames = NamesList . fmap NameText . Text.splitOn ":"
