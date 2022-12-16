{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Address where

import           Data.Text                       (Text)
import           Data.Foldable                   (asum)
import           Cardano.Api.Shelley             (AsType (..), StakeAddress(..),
                                                  deserialiseFromBech32)
import           Cardano.Chain.Common            (decodeAddressBase58)
import           Cardano.Ledger.Alonzo.TxInfo    (transKeyHash)
import           Cardano.Ledger.Address          (BootstrapAddress (BootstrapAddress), bootstrapKeyHash)
import qualified Cardano.Ledger.Credential       as Shelley
import           Cardano.Ledger.Crypto           (StandardCrypto)
import           Control.FromSum                 (eitherToMaybe)
import           Ledger.Address                  (PaymentPubKeyHash(..), StakePubKeyHash(..), Address(..),
                                                    pubKeyHashAddress, toPubKeyHash, stakingCredential)
import           Ledger.Tx.CardanoAPI            (fromCardanoAddress)
import           Plutus.V1.Ledger.Credential     (Credential(..), StakingCredential(..))
import           PlutusTx.Prelude                hiding (asum, error)

---------------------------- Address to keyhashes conversions ----------------------------------

addressToKeyHashes :: Address -> Maybe (PaymentPubKeyHash, Maybe StakePubKeyHash)
addressToKeyHashes addr = do
    pkh  <- toPubKeyHash addr
    let skh = case stakingCredential addr of
            Just (StakingHash (PubKeyCredential spkh)) -> Just $ StakePubKeyHash spkh
            Just StakingPtr{}                          -> Nothing -- no support for pointers at the moment
            _                                          -> Nothing
    pure (PaymentPubKeyHash pkh, skh)

----------------------------------- Bech32 conversions -----------------------------------------

-- Extract key hashes from bech32 Shelley/Byron address
bech32ToKeyHashes :: Text -> Maybe (PaymentPubKeyHash, Maybe StakePubKeyHash)
bech32ToKeyHashes txt = do
    addr <- bech32ToAddress txt
    addressToKeyHashes addr

-- Convert bech32 Shelley/Byron address to Plutus Address
bech32ToAddress :: Text -> Maybe Address
bech32ToAddress txt = asum $ map ($ txt) [bech32ToShelley, bech32ToByron]

bech32ToShelley :: Text -> Maybe Address
bech32ToShelley txt = do
    sAddr <- eitherToMaybe (deserialiseFromBech32 AsShelleyAddress txt)
    Just $ fromCardanoAddress sAddr

bech32ToByron :: Text -> Maybe Address
bech32ToByron txt =  fromByron <$> eitherToMaybe (decodeAddressBase58 txt)
    where
        fromByron = flip pubKeyHashAddress Nothing . PaymentPubKeyHash . transKeyHash . bootstrapKeyHash @StandardCrypto . BootstrapAddress

-- Convert bech32 Stake address to a Plutus StakePubKeyHash.
bech32ToStakePubKeyHash :: Text -> Maybe StakePubKeyHash
bech32ToStakePubKeyHash txt = do
    StakeAddress _ payCred <- either (const Nothing) Just $ deserialiseFromBech32 AsStakeAddress txt
    case payCred of
            Shelley.ScriptHashObj _  -> Nothing
            Shelley.KeyHashObj kHash -> Just $ StakePubKeyHash $ transKeyHash kHash