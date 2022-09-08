{-# LANGUAGE DataKinds #-}

module Utils.Passphrase where

import           Cardano.Wallet.Primitive.Passphrase                (Passphrase (..))
import           Data.Text.Class                                    (FromText(fromText), ToText(toText), TextDecodingError)

convertPassphrase :: Passphrase "user" -> Either TextDecodingError (Passphrase "lenient")
convertPassphrase = fromText . toText