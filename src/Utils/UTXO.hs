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


module Utils.UTXO where

import           Data.Map                          (Map, empty, filterWithKey, keys)
import           Ledger                            (ChainIndexTxOut(..), TxOutRef(..))
import           PlutusTx.Prelude                  hiding ((<>), null)
import           Prelude                           (null)

selectUTXO :: Map TxOutRef ChainIndexTxOut -> Map TxOutRef ChainIndexTxOut
selectUTXO utxos
    | null utxos = empty
    | otherwise  = filterWithKey (\k _ -> k == key) utxos
  where key = head $ keys utxos