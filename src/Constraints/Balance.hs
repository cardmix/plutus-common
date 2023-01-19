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
import           Data.Default                 (Default(..))
import           Data.Either.Extra            (eitherToMaybe)
import           Data.Map                     (difference)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import           Ledger                       (CardanoTx (..), SomeCardanoApiTx (..), pubKeyHashAddress, toTxOut)
import           Ledger.Constraints           (UnbalancedTx (..), ScriptLookups (..), mkTxWithParams)
import           Ledger.Fee                   (makeAutoBalancedTransaction)
import           Ledger.Index                 (UtxoIndex(..))
import           Ledger.Params                (Params (..))
import           Ledger.Tx.CardanoAPI         (toCardanoTxBodyContent)
import           Ledger.Validation            (UTxO, EmulatorEra, fromPlutusIndex)
import           Prelude

import           Constraints.CoinSelection    (CoinSelectionParams, CoinSelectionBudget)
import           Constraints.OffChain         (prebalanceTx)
import           Types.Tx                     (TransactionBuilder, Transaction, TxConstructor (..), constructTxConstraints)
import           Utils.ChainIndex             (MapUTXO)

-- A helper function to convert our MapUTXO type to UTxO type from Ledger.Validation
-- TODO: add error handling
toCardanoUTXO :: Params -> MapUTXO -> UTxO EmulatorEra
toCardanoUTXO params utxos =
        let index = UtxoIndex $ Map.map (fromJust . eitherToMaybe . toTxOut (pNetworkId params)) utxos
        in fromJust $ eitherToMaybe $ fromPlutusIndex index

-- TODO: add error handling
constructCardanoTx :: Params -> CoinSelectionBudget -> CoinSelectionParams ->
    TransactionBuilder () -> Transaction -> Maybe CardanoTx
constructCardanoTx params coinBudget coinParams builder txInit = do
    let builder'    = builder >> prebalanceTx coinBudget coinParams
        buildResult = constructTxConstraints builder' txInit
    (lookups, cons) <- buildResult
    utx             <- eitherToMaybe $ mkTxWithParams def lookups cons
    cardanoBuildTx  <-  case utx of
        UnbalancedEmulatorTx etx _ _ -> eitherToMaybe $ toCardanoTxBodyContent params [] etx
        UnbalancedCardanoTx cbt _    -> pure cbt
    let (pkh, skh)  = txCreator txInit
        changeAddr  = pubKeyHashAddress pkh skh
        utxosCardano = toCardanoUTXO params (txConstructorLookups txInit `difference` slTxOutputs lookups)
    tx <- eitherToMaybe $ makeAutoBalancedTransaction params utxosCardano cardanoBuildTx changeAddr
    return $ CardanoApiTx $ SomeTx tx BabbageEraInCardanoMode