{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Utils.Tx where

import           Cardano.Api.Shelley     (serialiseToCBOR)
import           Data.Aeson.Extras       (encodeByteString)
import           Data.Default            (def)
import           Data.Text               (Text)
import           Ledger.Constraints      (UnbalancedTx)
import           Ledger.Tx.CardanoAPI    (ToCardanoError)
import           Plutus.Contract.Wallet  (ExportTx (..), export)
import           PlutusTx.Prelude        hiding ((<>))
import           Prelude                 (undefined)

import           Configuration.PABConfig

------------------------ Export/Import of transactions -------------------------

-- UnbalancedTx to CBOR conversion
unbalancedTxToCBOR :: UnbalancedTx -> Either ToCardanoError Text
unbalancedTxToCBOR = fmap (encodeByteString . serialiseToCBOR . partialTx) . f
    where f utx = case pabConfig of
            PABMainnet -> undefined
            PABTestnet -> export testnetParams testnetId def utx
