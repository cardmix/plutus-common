{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Constraints.Balance where

import           Cardano.Api                  (EraInMode(..))
import           Data.Either.Extra            (eitherToMaybe)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           Ledger                       (CardanoTx (..), SomeCardanoApiTx (..), toTxOut, Address)
import           Ledger.Constraints           (UnbalancedTx (..), ScriptLookups (..), mkTxWithParams, TxConstraints)
import           Ledger.Fee                   (makeAutoBalancedTransaction)
import           Ledger.Index                 (UtxoIndex(..))
import           Ledger.Params                (Params (..))
import           Ledger.Tx.CardanoAPI         (toCardanoTxBodyContent)
import           Ledger.Typed.Scripts         (ValidatorTypes(..), Any)
import           Ledger.Validation            (UTxO, EmulatorEra, fromPlutusIndex)
import           Prelude

import           Constraints.CoinSelection    (CoinSelectionParams, CoinSelectionBudget)
import           Constraints.OffChain         (prebalanceTx)
import           Types.Tx                     (TransactionBuilder)
import           Utils.ChainIndex             (MapUTXO)

-- A helper function to convert our MapUTXO type to UTxO type from Ledger.Validation
-- TODO: add better error handling
toCardanoUTXO :: Params -> MapUTXO -> UTxO EmulatorEra
toCardanoUTXO params utxos =
        let handleError msg = fromMaybe (error msg) . eitherToMaybe
            msg1 = "toTxOut: Failed to convert a DecoratedTxOut to TxOut."
            msg2 = "fromPlutusIndex: Failed to convert a UtxoIndex to UTxO EmulatorEra."
            index = UtxoIndex $ Map.map (handleError msg1 . toTxOut (pNetworkId params)) utxos
        in handleError msg2 $ fromPlutusIndex index

addPrebalanceTx :: CoinSelectionBudget -> CoinSelectionParams -> MapUTXO -> TransactionBuilder () -> TransactionBuilder ()
addPrebalanceTx budget params utxos builder = builder >> prebalanceTx budget params utxos

-- TODO: add better error handling
balanceExternalTx :: (Monad m) => Params
                  -> ScriptLookups Any
                  -> TxConstraints (RedeemerType Any) (DatumType Any)
                  -> MapUTXO
                  -> Address
                  -> m CardanoTx
balanceExternalTx params lookups cons walletUTXO changeAddress = do
    let handleError msg = fromMaybe (error msg) . eitherToMaybe
        msg1 = "mkTxWithParams: Cannot make an UnbalancedTx."
        msg2 = "toCardanoTxBodyContent: Cannot get a CardanoBuildTx from Tx."
        msg3 = "makeAutoBalancedTransaction: Cannot auto-balance a transaction."

    let utx = handleError msg1 $ mkTxWithParams params lookups cons
        cardanoBuildTx = case utx of
            UnbalancedEmulatorTx etx _ _ -> handleError msg2 $ toCardanoTxBodyContent params [] etx
            UnbalancedCardanoTx cbt _    -> cbt
        utxosCardano = toCardanoUTXO params walletUTXO
        tx = handleError msg3 $ makeAutoBalancedTransaction params utxosCardano cardanoBuildTx changeAddress
    return $ CardanoApiTx $ SomeTx tx BabbageEraInCardanoMode