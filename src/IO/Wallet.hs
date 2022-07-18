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

import           Cardano.Api (NetworkId)
import           Cardano.Api.ProtocolParameters (ProtocolParameters)
import           Data.Default                      (Default(..))
import           Data.Either                       (fromRight)
import           Ledger                            (CardanoTx)
import           Ledger.Constraints                (TxConstraints, ScriptLookups, mkTx)
import           Ledger.Typed.Scripts              (ValidatorTypes(..))
import           Plutus.Contract.Wallet            (export)
import           Plutus.V1.Ledger.Api              (FromData, ToData)
import           PlutusTx.Prelude                   
import           Prelude                           (IO, undefined)


balanceTx :: (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a)) =>
    ProtocolParameters -> NetworkId -> ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> IO CardanoTx
balanceTx pparams networkID lookups cons = undefined
    where _ = fromRight (error ()) $ export pparams networkID def $ fromRight (error ()) $ mkTx lookups cons -- tx to pass to the wallet as JSON

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: CardanoTx -> IO ()
submitTx = undefined

-- Send a balanced transaction to Cardano Wallet Backend and wait until transaction is confirmed or declined
submitTxConfirmed :: CardanoTx -> IO ()
submitTxConfirmed = undefined