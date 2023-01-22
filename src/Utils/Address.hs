{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Utils.Address where

import           Data.Text                       (Text)
import           Cardano.Api.Shelley             (AsType(..), StakeAddress(..), shelleyAddressInEra, ShelleyEra, SerialiseAddress(..), 
                                                  byronAddressInEra)
import           Cardano.Ledger.Alonzo.TxInfo    (transKeyHash)
import qualified Cardano.Ledger.Credential       as Shelley
import           Control.Applicative             ((<|>))
import           Ledger                          (StakingCredential, toPlutusAddress)
import           Ledger.Address                  (PaymentPubKeyHash(..), StakePubKeyHash(..), Address(..), toPubKeyHash, stakingCredential)

---------------------------- Address to keyhashes conversions ----------------------------------

addressToKeyHashes :: Address -> Maybe (PaymentPubKeyHash, Maybe StakingCredential)
addressToKeyHashes addr = do
    pkh  <- toPubKeyHash addr
    pure (PaymentPubKeyHash pkh, stakingCredential addr)

----------------------------------- Bech32 conversions -----------------------------------------

-- TODO: simplify address conversions using the new Plutus.Ledger functions

-- Extract key hashes from bech32 Shelley/Byron address
bech32ToKeyHashes :: Text -> Maybe (PaymentPubKeyHash, Maybe StakingCredential)
bech32ToKeyHashes txt = do
    addr <- bech32ToAddress txt
    addressToKeyHashes addr

-- Convert bech32 Shelley/Byron address to Plutus Address
bech32ToAddress :: Text -> Maybe Address
bech32ToAddress txt = toPlutusAddress <$> (shelleyAddr <|> byronAddr)
    where
        shelleyAddr = shelleyAddressInEra @ShelleyEra <$> deserialiseAddress AsShelleyAddress txt
        byronAddr = byronAddressInEra <$> deserialiseAddress AsByronAddress txt

-- Convert bech32 Stake address to a Plutus StakePubKeyHash.
bech32ToStakePubKeyHash :: Text -> Maybe StakePubKeyHash
bech32ToStakePubKeyHash txt = do
    StakeAddress _ payCred <- deserialiseAddress AsStakeAddress txt
    case payCred of
            Shelley.ScriptHashObj _  -> Nothing
            Shelley.KeyHashObj kHash -> Just $ StakePubKeyHash $ transKeyHash kHash