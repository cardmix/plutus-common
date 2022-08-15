{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Wallet where

import Cardano.Api.Shelley hiding (Address)
-- import Cardano.Api.Address
import Data.Aeson
import Data.ByteString.Lazy
import Data.Default
import Data.FileEmbed
import Data.Maybe
import Data.Text
import IO.Wallet
import Ledger
import Utils.Address

protocolParams :: ProtocolParameters
protocolParams = fromJust $ decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 1097911063

ledgerParams :: Params
ledgerParams = Params def protocolParams networkId

daedalusAddress :: Text
daedalusAddress = "addr_test1qpmv0wkr6z9sdqveecpuywrwcxyueft0wgle85cs9fhsvtgnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdq7aezrw"

test :: IO [TxOutRef]
test = let Just (paymentPKH, Just stakePKH) = bech32ToKeyHashes daedalusAddress
       in getWalletTxOutRefs ledgerParams paymentPKH stakePKH 1
