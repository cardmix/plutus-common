{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Utils.Crypto where

import           Crypto.Error                 (throwCryptoError)
import qualified Crypto.PubKey.Ed25519        as Crypto
import           Data.ByteArray               (unpack)
import           Data.ByteString              (pack)
import           PlutusTx.Prelude             (BuiltinByteString, toBuiltin, verifyEd25519Signature)

sign :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString
sign prvKey msg = toBuiltin . pack . unpack $ sig
    where s   = throwCryptoError $ Crypto.secretKey prvKey
          pk  = Crypto.toPublic s
          sig = Crypto.sign s pk msg

verify :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> Bool
verify = verifyEd25519Signature