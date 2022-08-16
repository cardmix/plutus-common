{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module IO.Wallet where

import           Cardano.Api                               (toEraInMode, ConsensusMode(..), InAnyCardanoEra(..), CardanoEra (..), AnyCardanoEra (..))
import qualified Cardano.Wallet.Api.Client                 as Client
import           Cardano.Wallet.Api.Types                  (ApiSerialisedTransaction(..), ApiT(..), ApiTxId(..), ApiSignTransactionPostData(ApiSignTransactionPostData))
import           Cardano.Wallet.Api.Types.SchemaMetadata   (TxMetadataSchema(..))
import           Cardano.Wallet.Primitive.Types            (WalletId(..))
import           Cardano.Wallet.Primitive.Passphrase.Types (Passphrase)
import           Cardano.Wallet.Primitive.Types.Tx         (sealedTxFromCardano', SealedTx(..), cardanoTxIdeallyNoLaterThan)
import           Control.Concurrent                        (threadDelay)
import           Control.Lens                              ((^?))
import           Control.Monad                             (void, unless)
import           Data.Aeson                                (ToJSON(toJSON))
import           Data.Aeson.Lens                           (key, AsPrimitive(_String))
import           Data.Either                               (fromRight)
import           Data.Map                                  (keys)
import           Data.Maybe                                (fromJust)
import           Data.Text                                 (pack)
import           Data.Text.Class                           (FromText(fromText))
import           Data.Void                                 (Void)
import           Ledger                                    (CardanoTx (..), Params (..), PaymentPubKeyHash, StakePubKeyHash, TxOutRef)
import           Ledger.Ada                                (lovelaceValueOf)
import           Ledger.Constraints                        (TxConstraints, ScriptLookups, mkTx, mustPayToPubKeyAddress)
import           Ledger.Typed.Scripts                      (ValidatorTypes(..))
import           Ledger.Tx                                 (getCardanoTxId)
import           Ledger.Tx.CardanoAPI                      (unspentOutputsTx, SomeCardanoApiTx(..))
import           Plutus.Contract.Wallet                    (export)
import           PlutusTx.IsData                           (ToData, FromData)
import           PlutusTx.Prelude                          hiding (mempty, pure, (<$>), unless)
import           Prelude                                   (IO, mempty, (<$>), Show (..), print, String)
import           Utils.Prelude                             (replicate)
import qualified Utils.Servant                             as Servant


getFromEndpoint :: Servant.Endpoint a
getFromEndpoint = Servant.getFromEndpointOnPort 8090

walletIdHardcoded :: WalletId
walletIdHardcoded = fromRight (error ()) $ fromText "0a595ab16d8e23b33cec2381d0a385a571cfc33b"

passphraseHardcoded :: Passphrase "lenient"
passphraseHardcoded = fromRight (error ()) $ fromText "1234567890"

-- getWalletFromId :: WalletId -> IO ApiWallet
-- getWalletFromId wid = getFromEndpoint $ Client.getWallet Client.walletClient $ ApiT wid

signTx :: CardanoTx -> IO ApiSerialisedTransaction
signTx tx = getFromEndpoint $ Client.signTransaction Client.transactionClient
    (ApiT walletIdHardcoded)
    (ApiSignTransactionPostData (ApiT $ cardanoTxToSealedTx tx) (ApiT passphraseHardcoded))

balanceTx :: (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a)) =>
    Params -> ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> IO CardanoTx
balanceTx params lookups cons = apiSerializedTxToCardanoTx <$> do
    print $ toJSON etx
    getFromEndpoint $ Client.balanceTransaction Client.transactionClient
        (ApiT walletIdHardcoded)
        (toJSON etx)
    where etx = fromRight (error ()) $ export params $ fromRight (error ()) $ mkTx lookups cons -- tx to pass to the wallet as JSON

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: CardanoTx -> IO ()
submitTx ctx = void $ getFromEndpoint $ Client.submitTransaction Client.transactionClient
    (ApiT walletIdHardcoded)
    (ApiSerialisedTransaction $ ApiT $ cardanoTxToSealedTx ctx)

-- Send a balanced transaction to Cardano Wallet Backend and wait until transaction is confirmed or declined
submitTxConfirmed :: CardanoTx -> IO ()
submitTxConfirmed ctx = submitTx ctx >> awaitTxConfirmed ctx

-- Wait until a transaction is confirmed (added to the ledger).
-- If the transaction is never added to the ledger then 'awaitTxConfirmed' never
-- returns
awaitTxConfirmed :: CardanoTx -> IO ()
awaitTxConfirmed ctx = go
    where
        go = do
            res <- getFromEndpoint $ Client.getTransaction Client.transactionClient
                (ApiT walletIdHardcoded)
                (ApiTxId $ ApiT $ mkHash ctx)
                TxMetadataNoSchema
            unless (confirmedResponse res) $ threadDelay 1_000_000 >> go
        confirmedResponse res = case res ^? key "status"._String of
            Just "in_ledger" -> True
            _                -> False
        mkHash = fromRight (error ()) . fromText . pack . show  . getCardanoTxId

-- Should these two things be moved to Utils?
cardanoTxToSealedTx :: CardanoTx -> SealedTx
cardanoTxToSealedTx = \case
    (CardanoApiTx (SomeTx tx _)) -> sealedTxFromCardano' tx
    (Both _ (SomeTx tx _))       -> sealedTxFromCardano' tx
    _                            -> error ()

apiSerializedTxToCardanoTx :: ApiSerialisedTransaction -> CardanoTx
apiSerializedTxToCardanoTx = CardanoApiTx . toSomeTx . toAnyEraTx
    where
        toAnyEraTx = cardanoTxIdeallyNoLaterThan (AnyCardanoEra BabbageEra) . getApiT . transaction
        toSomeTx (InAnyCardanoEra cera tx) = SomeTx tx $ fromJust $ toEraInMode cera CardanoMode

-- Create and submit a transaction that produces a specific number of outputs at the target wallet address
getWalletTxOutRefs :: Params -> PaymentPubKeyHash -> StakePubKeyHash -> Integer -> IO [TxOutRef]
getWalletTxOutRefs params pkh skh n = do
        print ("Balancing..." :: String)
        balancedTx <- balanceTx params lookups cons
        print balancedTx
        print ("Signing..." :: String)
        signedTx <- apiSerializedTxToCardanoTx <$> signTx balancedTx
        print signedTx
        print ("Submitting..." :: String)
        submitTxConfirmed signedTx
        let refs = case signedTx of
                EmulatorTx _    -> error ()
                CardanoApiTx tx -> keys $ unspentOutputsTx tx
                Both _ tx       -> keys $ unspentOutputsTx tx
        print ("Submitted!" :: String)
        return refs
    where
        lookups = mempty :: ScriptLookups Void
        cons    = mconcat $ replicate n $ mustPayToPubKeyAddress pkh skh $ lovelaceValueOf 10_000_000