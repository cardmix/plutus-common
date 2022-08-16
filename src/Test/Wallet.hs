{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Wallet where

import Cardano.Api.Shelley   (NetworkId(..), ProtocolParameters (..), NetworkMagic(..))
import Data.Aeson            (decode)
import Data.ByteString.Lazy  (readFile)
import Data.Default          (Default(..))
import Data.Maybe            (fromJust)
import Data.Text             (Text)
import Ledger                (Params(..), TxOutRef)
import Prelude               hiding (readFile)

import IO.Wallet             (getWalletTxOutRefs)
import Utils.Address         (bech32ToKeyHashes)

daedalusAddress :: Text
daedalusAddress = "addr_test1qpmv0wkr6z9sdqveecpuywrwcxyueft0wgle85cs9fhsvtgnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdq7aezrw"

test :: IO [TxOutRef]
test = do
  pp <- fromJust . decode <$> readFile "testnet/protocol-parameters.json" :: IO ProtocolParameters
  let (pkh, skh) = fromJust $ bech32ToKeyHashes daedalusAddress
      networkId = Testnet $ NetworkMagic 1097911063
      ledgerParams = Params def pp networkId
  getWalletTxOutRefs ledgerParams pkh (fromJust skh) 1
