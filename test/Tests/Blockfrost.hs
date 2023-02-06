{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Tests.Blockfrost where

import           Cardano.Api                   (AsType (..), Hash, NetworkId (..), NetworkMagic (..), SerialiseAsRawBytes (..),
                                                StakeAddress (..), StakeKey, TxId, deserialiseFromBech32)
import           Cardano.Api.Shelley           (PoolId, StakeAddress (..), StakeCredential (..))
import           Cardano.Ledger.Alonzo.TxInfo  (transKeyHash)
import qualified Cardano.Ledger.Credential     as Cred
import           Cardano.Ledger.Crypto         (StandardCrypto)
import           Cardano.Ledger.Keys           (KeyHash (..))
import           Data.Maybe                    (fromJust)
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import           Ledger                        (Address, MintingPolicyHash (..), PubKeyHash (..),
                                                StakePubKeyHash (StakePubKeyHash), stakePubKeyHashCredential, stakingCredential)
import           Plutus.V1.Ledger.Api          (fromBuiltin, toBuiltin)
import           PlutusAppsExtra.IO.Blockfrost (getAddressFromStakePubKeyHash, verifyAsset)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress, bech32ToStakeAddress)
import qualified Text.Hex                      as T

toStake :: Text -> StakeAddress
toStake = fromJust . bech32ToStakeAddress 

toPool :: Text -> PoolId
toPool txt = let Right pool = deserialiseFromBech32 (AsHash AsStakePoolKey) txt in pool

toAddr :: Text -> Address
toAddr =  fromJust . bech32ToAddress 

stake1, stake2 :: StakeAddress
stake1 = toStake "stake_test1uqxnjdhxpt0p22qzu34jk7f9wdj4mxgvddafr65dttmj2scltfs6h"
stake2 = toStake "stake_test1uqzgefvrhxyjhhvvxq0yu2dp85qj8ptg32cq409x0naq2ugpyl5kc"

pool1, pool2 :: PoolId
pool1 = toPool "pool1vzqtn3mtfvvuy8ghksy34gs9g97tszj5f8mr3sn7asy5vk577ec"
pool2 = toPool "pool10c40pnzz3e00kuej05xfcs2ptkekhys48q7qc4jjcsysypj46qv"

addr1, addr2 :: Address
addr1 = toAddr "addr_test1qrrrq09fn5dxashl6rhppk5rdxsfq5zlqv2yt2c22dul73cd8ymwvzk7z55q9ert9duj2um9tkvsc6m6j84g6khhy4psaynw56"
addr2 = toAddr "addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99"

test1, test2 :: IO ()
test1 = getAddress stake1 pool1 addr1
test2 = getAddress stake2 pool2 addr2

getAddress :: StakeAddress -> PoolId -> Address -> IO ()
getAddress (StakeAddress _ s@(Cred.KeyHashObj hash)) pool addr
    = getAddressFromStakePubKeyHash (Testnet $ NetworkMagic 2) pool (stakeCredToSpkh s) >>= \case
        Just addr' -> if addr == addr' then print True else failedTest addr addr'
    where 
        failedTest a a' = print $ "expected:" <> show a <> " got: " <> show a'

stakeCredToSpkh :: Cred.StakeCredential StandardCrypto -> StakePubKeyHash
stakeCredToSpkh (Cred.KeyHashObj hash) = StakePubKeyHash $ transKeyHash hash

verifyAssetTest :: IO (Maybe TxId)
verifyAssetTest = verifyAsset 
    (MintingPolicyHash $ toBuiltin $ fromJust$ T.decodeHex "4cd1187e477d56e419c354f1e4c7997a736dfc5e095a2511aba0f75d")
    1 
    (toAddr "addr_test1qznvz33axk8zxup2e2wgt7zr0398r3x8uup5xf8ljddreqpu9sytuyrjjxlg6udmkvk6z8emjasmpxgl9fhkjs857wgqrfjuwn")