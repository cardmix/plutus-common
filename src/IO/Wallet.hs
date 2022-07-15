{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module IO.Wallet where

import           Ledger                            (CardanoTx)
import           Ledger.Constraints                (TxConstraints, ScriptLookups)
import           Prelude                           (IO, undefined)


balanceTx :: (ScriptLookups a, TxConstraints i o) -> IO CardanoTx
balanceTx = undefined

-- Send a balanced transaction to Cardano Wallet Backend
submitTx :: CardanoTx -> IO ()
submitTx = undefined

submitTxConfirmed :: CardanoTx -> IO ()
submitTxConfirmed = undefined