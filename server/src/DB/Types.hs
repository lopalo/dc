
module DB.Types where

import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

import Data.Aeson (Value, decodeStrict', encode)
import Data.Aeson.Types (Parser, parseMaybe)


type DBName = String


class Persistent p where

    toDB :: p -> Value

    fromDB :: Value -> Parser p

    toByteString :: p -> ByteString
    toByteString = toStrict . encode . toDB

    fromByteString :: ByteString -> p
    fromByteString bs = fromJust $ decodeStrict' bs >>= parseMaybe fromDB


