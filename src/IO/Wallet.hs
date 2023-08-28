{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE DerivingStrategies           #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE LambdaCase                   #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE NumericUnderscores           #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ViewPatterns                 #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>"          #-}

module IO.Wallet where

import qualified Cardano.Wallet.Api.Client                          as Client
import           Cardano.Wallet.Api.Types                           (ApiSerialisedTransaction(..), ApiT(..), ApiTxId(..), 
                                                                     ApiSignTransactionPostData(ApiSignTransactionPostData))
import           Cardano.Wallet.Api.Types.SchemaMetadata            (TxMetadataSchema(..))
import           Cardano.Mnemonic                                   (SomeMnemonic, MkSomeMnemonic(..))
import           Cardano.Wallet.Primitive.AddressDerivation         (WalletKey(digest, publicKey))
import           Cardano.Wallet.Primitive.AddressDerivation.Shelley (generateKeyFromSeed)
import           Cardano.Wallet.Primitive.Passphrase                (Passphrase (..), currentPassphraseScheme, preparePassphrase)
import           Cardano.Wallet.Primitive.Types                     (WalletId(..))
import           Control.Concurrent                                 (threadDelay)
import           Control.FromSum                                    (fromEither)
import           Control.Lens                                       ((^?))
import           Control.Monad                                      (void, unless, MonadFail (fail))
import           Data.Aeson                                         (FromJSON(..), ToJSON(..), (.:), eitherDecode, withObject)
import           Data.Aeson.Lens                                    (key, AsPrimitive(_String))
import qualified Data.ByteString.Lazy                               as LB
import           Data.Functor                                       ((<&>))
import           Data.Either                                        (fromRight)
import           Data.Map                                           (keys)
import           Data.String                                        (IsString(..))
import           Data.Text                                          (Text)
import qualified Data.Text                                          as T
import           Data.Text.Class                                    (FromText(fromText))
import           Data.Void                                          (Void)
import           GHC.Generics                                       (Generic)
import           Ledger                                             (CardanoTx (..), Params (..), PaymentPubKeyHash, StakePubKeyHash, TxOutRef)
import           Ledger.Ada                                         (lovelaceValueOf)
import           Ledger.Constraints                                 (TxConstraints, ScriptLookups, mkTx, mustPayToPubKeyAddress, mustPayToPubKey)
import           Ledger.Typed.Scripts                               (ValidatorTypes(..))
import           Ledger.Tx                                          (getCardanoTxId)
import           Ledger.Tx.CardanoAPI                               (unspentOutputsTx)
import           Plutus.Contract.Wallet                             (export)
import           PlutusTx.IsData                                    (ToData, FromData)
import           PlutusTx.Prelude                                   hiding (mempty, pure, (<$>), unless, error)
import           Prelude                                            (IO, mempty, (<$>), Show (..), print, String, Applicative (pure), error, FilePath)
import           Utils.Prelude                                      (replicate)
import qualified Utils.Servant                                      as Servant
import           Utils.Tx                                           (apiSerializedTxToCardanoTx, cardanoTxToSealedTx)


getFromEndpoint :: Servant.Endpoint a
getFromEndpoint = Servant.getFromEndpointOnPort 8090

walletIdHardcoded :: WalletId
walletIdHardcoded = fromRight (error "Unparsable wallet id.") $ 
    fromText "0a595ab16d8e23b33cec2381d0a385a571cfc33b"

passphraseHardcoded :: Passphrase "lenient"
passphraseHardcoded = fromRight (error "Unparsable wallet passphrase.") $ 
    fromText "1234567890"

-- getWalletFromId :: WalletId -> IO ApiWallet
-- getWalletFromId wid = getFromEndpoint $ Client.getWallet Client.walletClient $ ApiT wid

signTx :: CardanoTx -> IO CardanoTx
signTx (cardanoTxToSealedTx -> Just stx) = apiSerializedTxToCardanoTx <$> sign >>= \case 
        Just ctx -> pure ctx
        _        -> error "Unable to convert ApiSerialisedTransaction to a CardanoTx."
    where
        sign = getFromEndpoint $ Client.signTransaction Client.transactionClient
            (ApiT walletIdHardcoded)
            (ApiSignTransactionPostData (ApiT stx) (ApiT passphraseHardcoded))
signTx _ = error "Unable to convert CardanoTx to a SealedTx."

balanceTx :: (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a)) =>
    Params -> ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> IO CardanoTx
balanceTx params lookups cons = apiSerializedTxToCardanoTx <$> balance >>= \case
    Just ctx -> pure ctx
    _        -> error "Unable to convert ApiSerialisedTransaction to a CardanoTx."
    where 
        -- tx to pass to the wallet as JSON
        etx = fromEither (error . show) $ export params $ fromEither (error . show) $ mkTx lookups cons
        balance = getFromEndpoint $ Client.balanceTransaction Client.transactionClient
            (ApiT walletIdHardcoded)
            (toJSON etx)

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: CardanoTx -> IO ()
submitTx (cardanoTxToSealedTx -> Just stx) = void $ getFromEndpoint $ 
    Client.submitTransaction Client.transactionClient
    (ApiT walletIdHardcoded)
    (ApiSerialisedTransaction $ ApiT stx)
submitTx _ = error "Unable to convert CardanoTx to a SealedTx."

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
        mkHash = fromEither (error . show) . fromText . T.pack . show  . getCardanoTxId

-- Create and submit a transaction that produces a specific number of outputs at the target wallet address
getWalletTxOutRefs :: Params -> PaymentPubKeyHash -> Maybe StakePubKeyHash -> Integer -> IO [TxOutRef]
getWalletTxOutRefs params pkh mbSkh n = do
        print ("Balancing..." :: String)
        balancedTx <- balanceTx params lookups cons
        print balancedTx
        print ("Signing..." :: String)
        signedTx <- signTx balancedTx
        print signedTx
        print ("Submitting..." :: String)
        submitTxConfirmed signedTx
        let refs = case signedTx of
                EmulatorTx _    -> error "Can not get TxOutRef's from EmulatorTx."
                CardanoApiTx tx -> keys $ unspentOutputsTx tx
                Both _ tx       -> keys $ unspentOutputsTx tx
        print ("Submitted!" :: String)
        return refs
    where
        lookups = mempty :: ScriptLookups Void
        cons    = case mbSkh of
            Just skh -> mconcat $ replicate n $ mustPayToPubKeyAddress pkh skh $ lovelaceValueOf 10_000_000
            Nothing -> mustPayToPubKey pkh $ lovelaceValueOf 10_000_000

-- Restore-wallet JSON file content
data RestoreWallet = RestoreWallet
    { name :: Text
    , mnemonicSentence :: SomeMnemonic
    , passphrase :: Passphrase "user"
    } deriving Generic

instance FromJSON RestoreWallet where
    parseJSON = withObject "Restore wallet" $ \v -> do 
        name                   <- v .: "name"
        mnemonicSentence       <- v .: "mnemonic_sentence" >>= either (fail . show) pure . mkSomeMnemonic @'[ 9, 12, 15, 18, 21, 24 ]
        passphrase             <- v .: "passphrase"        <&> Passphrase . fromString . T.unpack
        pure RestoreWallet{..}

genWalletId :: SomeMnemonic -> Passphrase "user" -> WalletId
genWalletId mnemonic pwd = WalletId $ digest $ publicKey rootXPrv
  where
    rootXPrv = generateKeyFromSeed (mnemonic, Nothing) pwdP
    pwdP = preparePassphrase currentPassphraseScheme pwd

-- Read restore-wallet JSON file and generate walletId from it
walletIdFromFile :: FilePath -> IO WalletId
walletIdFromFile fp = eitherDecode <$> LB.readFile fp >>= \case
    Right RestoreWallet{..} -> pure $ genWalletId mnemonicSentence passphrase
    Left err                -> error err