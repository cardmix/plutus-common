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
import           Cardano.Api.Shelley             (AsType (..), StakeAddress(..), StakeAddressReference (..), StakeCredential (..), Hash (..),
                                                  deserialiseFromBech32, fromShelleyStakeReference)
import qualified Cardano.Api.Shelley             as Shelley
import           Cardano.Chain.Common            (decodeAddressBase58)
import           Cardano.Ledger.Alonzo.TxInfo    (transKeyHash)
import           Cardano.Ledger.Address          (bootstrapKeyHash, BootstrapAddress (BootstrapAddress))
import           Cardano.Ledger.Credential       (Credential(..))
import qualified Cardano.Ledger.Credential       as Shelley
import           Cardano.Ledger.Crypto           (StandardCrypto)
import           Control.FromSum                 (eitherToMaybe)
import           Ledger.Address                  (PaymentPubKeyHash(..), StakePubKeyHash(..), Address(..), pubKeyHashAddress, toPubKeyHash, stakingCredential)
import           Plutus.V1.Ledger.Credential     (Credential(..), StakingCredential(..))
import           PlutusTx.Prelude                hiding (asum, error)

-- Extract key hashes from bech32 Shelley/Byron address
bech32ToKeyHashes :: Text -> Maybe (PaymentPubKeyHash, Maybe StakePubKeyHash)
bech32ToKeyHashes txt = do
    addr <- bech32ToAddress txt
    pkh  <- toPubKeyHash addr
    let skh = case stakingCredential addr of
            Just (StakingHash (PubKeyCredential spkh)) -> Just $ StakePubKeyHash spkh
            Just (StakingHash (ScriptCredential vh))   -> Nothing
            Just (StakingPtr{})                        -> Nothing -- yet
            Nothing                                    -> Nothing
    pure (PaymentPubKeyHash pkh, skh)

-- Convert bech32 Shelley/Byron address to Plutus Address
bech32ToAddress :: Text -> Maybe Address
bech32ToAddress txt = asum $ map ($ txt) [bech32ToShelley, bech32ToByron]

bech32ToShelley :: Text -> Maybe Address
bech32ToShelley txt = do
    Shelley.ShelleyAddress _ payCred stakeRef  <- eitherToMaybe $ deserialiseFromBech32 AsShelleyAddress txt
    pkh <- case payCred of
            KeyHashObj    h1 -> Just $ PaymentPubKeyHash $ transKeyHash h1
            ScriptHashObj _  -> Nothing
    let skh = case fromShelleyStakeReference stakeRef of
            StakeAddressByValue (StakeCredentialByKey (StakeKeyHash h2)) -> Just $ StakePubKeyHash $ transKeyHash h2
            _  -> Nothing
    pure $ pubKeyHashAddress pkh skh

bech32ToByron :: Text -> Maybe Address
bech32ToByron txt =  do 
    bAddr <- eitherToMaybe $ decodeAddressBase58 txt
    pure $ flip pubKeyHashAddress Nothing $ PaymentPubKeyHash $ transKeyHash $ bootstrapKeyHash @StandardCrypto $ BootstrapAddress bAddr  

-- Convert bech32 Stake address to a Plutus StakePubKeyHash.
bech32ToStakePubKeyHash :: Text -> Maybe StakePubKeyHash
bech32ToStakePubKeyHash txt = do
    StakeAddress _ payCred <- either (const Nothing) Just $ deserialiseFromBech32 AsStakeAddress txt
    case payCred of
            Shelley.ScriptHashObj _ -> Nothing
            Shelley.KeyHashObj kHash -> Just $ StakePubKeyHash $ transKeyHash kHash
