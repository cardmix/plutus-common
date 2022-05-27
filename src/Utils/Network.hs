{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Utils.Network where

import           Data.Aeson              (decode)
import           Data.ByteString.Lazy    (fromStrict)
import           Data.Maybe              (fromJust)
import           Cardano.Api.Shelley     (ProtocolParameters, NetworkId(..), NetworkMagic (..))
import           Data.FileEmbed          (embedFile)
import           PlutusTx.Prelude        

data NetworkConfig = NetworkConfigMainnet | NetworkConfigTestnet

testnetId :: NetworkId
testnetId = Testnet $ NetworkMagic 1097911063

testnetParams :: ProtocolParameters
testnetParams = fromJust $ decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")