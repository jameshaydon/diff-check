module Hash where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Protolude hiding (hash)

sha256_8 :: Text -> Text
sha256_8 chunk =
  let digest :: ByteString = convertToBase Base64 (hashWith SHA256 (encodeUtf8 chunk))
   in T.take 8 (toS digest)
