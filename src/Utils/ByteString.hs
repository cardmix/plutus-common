{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Utils.ByteString where

import           Ledger.Address                    (Address (..), PaymentPubKeyHash (..))
import           Plutus.V2.Ledger.Api              (Credential(..), PubKeyHash (..), ValidatorHash (..), StakingCredential (..))
import           PlutusTx.Prelude                  hiding ((<>))
import           Prelude                           (Char, String)

import           Utils.Prelude                     (drop)

class ToBuiltinByteString a where
    toBytes :: a -> BuiltinByteString

instance ToBuiltinByteString String where
    {-# INLINABLE toBytes #-}
    toBytes str = foldr (consByteString . g) emptyByteString (f str)
        where
            f s = if length s > 1 then take 2 s : f (drop 2 s) else []
            g s = charToHex (head s) * 16 + charToHex (s !! 1)

instance ToBuiltinByteString Bool where
    {-# INLINABLE toBytes #-}
    toBytes False = consByteString 0 emptyByteString
    toBytes True  = consByteString 1 emptyByteString

instance ToBuiltinByteString Integer where
    {-# INLINABLE toBytes #-}
    toBytes = integerToByteString

instance ToBuiltinByteString [Integer] where
    {-# INLINABLE toBytes #-}
    toBytes = foldr (appendByteString . toBytes) emptyByteString

instance ToBuiltinByteString Credential where
    {-# INLINABLE toBytes #-}
    toBytes (PubKeyCredential (PubKeyHash bs)) = bs
    toBytes (ScriptCredential (ValidatorHash bs)) = bs

instance ToBuiltinByteString Address where
    {-# INLINABLE toBytes #-}
    toBytes (Address cred1 sCred) = case sCred of
        Just (StakingHash cred2) -> toBytes cred1 `appendByteString` toBytes cred2
        Just (StakingPtr i1 i2 i3) -> toBytes cred1 `appendByteString` toBytes i1
            `appendByteString` toBytes i2 `appendByteString` toBytes i3
        Nothing -> toBytes cred1

instance ToBuiltinByteString PaymentPubKeyHash where
    {-# INLINABLE toBytes #-}
    toBytes (PaymentPubKeyHash (PubKeyHash bs)) = bs

{-# INLINABLE integerToByteString #-}
integerToByteString :: Integer -> BuiltinByteString
integerToByteString n = consByteString r $ if q > 0 then integerToByteString q else emptyByteString
    where (q, r) = divMod n 256

{-# INLINABLE byteStringToList #-}
byteStringToList :: BuiltinByteString -> [Integer]
byteStringToList bs = indexByteString bs 0 : byteStringToList (dropByteString 1 bs)

{-# INLINABLE byteStringToInteger #-}
byteStringToInteger :: BuiltinByteString -> Integer
byteStringToInteger bs = foldr (\d n -> 256*n + d) 0 (byteStringToList bs)

charToHex :: Char -> Integer
charToHex '0' = 0
charToHex '1' = 1
charToHex '2' = 2
charToHex '3' = 3
charToHex '4' = 4
charToHex '5' = 5
charToHex '6' = 6
charToHex '7' = 7
charToHex '8' = 8
charToHex '9' = 9
charToHex 'a' = 10
charToHex 'b' = 11
charToHex 'c' = 12
charToHex 'd' = 13
charToHex 'e' = 14
charToHex 'f' = 15
charToHex _   = error ()