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
import           Cardano.Node.Emulator        (Params (..))
import           Cardano.Node.Emulator.Fee    (makeAutoBalancedTransactionWithUtxoProvider, utxoProviderFromWalletOutputs)
import           Control.Monad.Catch          (MonadThrow(..))
import           Ledger                       (CardanoTx (..), SomeCardanoApiTx (..), Address)
import           Ledger.Constraints           (UnbalancedTx (..), ScriptLookups (..), mkTxWithParams, TxConstraints)
import           Ledger.Index                 (UtxoIndex(..))
import           Ledger.Tx.CardanoAPI         (toCardanoTxBodyContent, toCardanoAddressInEra)
import           Ledger.Typed.Scripts         (ValidatorTypes(..), Any)
import           Prelude
import           Types.Error                  (MkTxError(..), throwEither)
import           Utils.ChainIndex             (MapUTXO, toCardanoUtxo)

-- TODO: use different errors for each failable computation
balanceExternalTx :: (MonadThrow m)
                  => Params
                  -> MapUTXO
                  -> Address
                  -> ScriptLookups Any
                  -> TxConstraints (RedeemerType Any) (DatumType Any)
                  -> m CardanoTx
balanceExternalTx params walletUTXO changeAddress lookups cons = do
    utx <- throwEither UnbuildableTx $ mkTxWithParams params lookups cons

    cardanoBuildTx <- case utx of
            UnbalancedEmulatorTx etx _ _ -> throwEither UnbuildableTx $
                toCardanoTxBodyContent (pNetworkId params) (emulatorPParams params) [] etx
            UnbalancedCardanoTx cbt _    -> pure cbt

    utxoIndex <- UtxoIndex <$> toCardanoUtxo params (slTxOutputs lookups)

    cAddress <- throwEither UnbuildableTx $ toCardanoAddressInEra (pNetworkId params) changeAddress

    walletUTXOIndex <- toCardanoUtxo params walletUTXO

    let utxoProvider = throwEither UnbuildableTx . utxoProviderFromWalletOutputs walletUTXOIndex

    tx <- makeAutoBalancedTransactionWithUtxoProvider
            params
            utxoIndex
            cAddress
            utxoProvider
            (const $ throwM UnbuildableTx)
            cardanoBuildTx

    return $ CardanoApiTx $ SomeTx tx BabbageEraInCardanoMode