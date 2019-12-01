module Hash where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Protolude hiding (hash)
import System.IO (hFlush, stdout)

hashSha256 :: Text -> Text
hashSha256 chunk =
  let digest :: ByteString = convertToBase Base64 (hashWith SHA256 (encodeUtf8 chunk))
   in T.take 8 (toS digest)
