{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
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

import           Utils.Network

------------------------ Export/Import of transactions -------------------------

-- UnbalancedTx to CBOR conversion
unbalancedTxToCBOR :: NetworkConfig -> UnbalancedTx -> Either ToCardanoError Text
unbalancedTxToCBOR NetworkConfig {networkID, pparams} = fmap (encodeByteString . serialiseToCBOR . partialTx) . export pparams networkID def
