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
import           Data.Maybe                   (fromJust)
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
-- TODO: add error handling
toCardanoUTXO :: Params -> MapUTXO -> UTxO EmulatorEra
toCardanoUTXO params utxos =
        let index = UtxoIndex $ Map.map (fromJust . eitherToMaybe . toTxOut (pNetworkId params)) utxos
        in fromJust $ eitherToMaybe $ fromPlutusIndex index

addPrebalanceTx :: CoinSelectionBudget -> CoinSelectionParams -> MapUTXO -> TransactionBuilder () -> TransactionBuilder ()
addPrebalanceTx budget params utxos builder = builder >> prebalanceTx budget params utxos

-- TODO: add error handling
balanceExternalTx :: Params
                  -> ScriptLookups Any
                  -> TxConstraints (RedeemerType Any) (DatumType Any)
                  -> MapUTXO
                  -> Address
                  -> Maybe CardanoTx
balanceExternalTx params lookups cons walletUTXO changeAddress = do
    utx <- eitherToMaybe $ mkTxWithParams params lookups cons
    cardanoBuildTx <-  case utx of
        UnbalancedEmulatorTx etx _ _ -> eitherToMaybe $ toCardanoTxBodyContent params [] etx
        UnbalancedCardanoTx cbt _    -> pure cbt
    let utxosCardano = toCardanoUTXO params walletUTXO
    tx <- eitherToMaybe $ makeAutoBalancedTransaction params utxosCardano cardanoBuildTx changeAddress
    return $ CardanoApiTx $ SomeTx tx BabbageEraInCardanoMode