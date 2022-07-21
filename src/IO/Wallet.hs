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

import           Data.Either                       (fromRight)
import           Ledger                            (CardanoTx, Params)
import           Ledger.Constraints                (TxConstraints, ScriptLookups, mkTx)
import           Ledger.Typed.Scripts              (ValidatorTypes(..))
import           Plutus.Contract.Wallet            (export)
import           Plutus.V1.Ledger.Api              (FromData, ToData)
import           PlutusTx.Prelude                   
import           Prelude                           (IO, undefined)


balanceTx :: (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a)) =>
    Params -> ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> IO CardanoTx
balanceTx params lookups cons = undefined
    where _ = fromRight (error ()) $ export params $ fromRight (error ()) $ mkTx lookups cons -- tx to pass to the wallet as JSON

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: CardanoTx -> IO ()
submitTx = undefined

-- Send a balanced transaction to Cardano Wallet Backend and wait until transaction is confirmed or declined
submitTxConfirmed :: CardanoTx -> IO ()
submitTxConfirmed = undefined