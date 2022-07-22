{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module IO.Wallet where

import           Data.Either                       (fromRight)
import           Data.Map                          (keys)
import           Data.Void                         (Void)
import           Ledger                            (CardanoTx (..), Params, PaymentPubKeyHash, StakePubKeyHash, TxOutRef)
import           Ledger.Ada                        (lovelaceValueOf)
import           Ledger.Constraints                (TxConstraints, ScriptLookups, mkTx, mustPayToPubKeyAddress)
import           Ledger.Typed.Scripts              (ValidatorTypes(..))
import           Ledger.Tx.CardanoAPI              (unspentOutputsTx)
import           Plutus.Contract.Wallet            (export)
import           PlutusTx.IsData                   (ToData, FromData)
import           PlutusTx.Prelude                  hiding (mempty)
import           Prelude                           (IO, undefined, mempty)

import           Utils.Prelude                     (replicate)


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

-- Create and submit a transaction that produces a specific number of outputs at the target wallet address
getWalletTxOutRefs :: Params -> PaymentPubKeyHash -> StakePubKeyHash -> Integer -> IO [TxOutRef]
getWalletTxOutRefs params pkh skh n = do
        ctx <- balanceTx params lookups cons
        submitTxConfirmed ctx
        let refs = case ctx of
                EmulatorTx _    -> error ()
                CardanoApiTx tx -> keys $ unspentOutputsTx tx
                Both _ tx       -> keys $ unspentOutputsTx tx
        return refs
    where
        lookups = mempty :: ScriptLookups Void
        cons    = mconcat $ replicate n $ mustPayToPubKeyAddress pkh skh $ lovelaceValueOf 10_000_000