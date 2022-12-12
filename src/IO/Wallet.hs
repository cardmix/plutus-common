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

module IO.Wallet where

import qualified Cardano.Wallet.Api.Client                          as Client
import           Cardano.Wallet.Api.Types                           (ApiSerialisedTransaction(..), ApiT(..), ApiTxId(..),
                                                                     ApiSignTransactionPostData(ApiSignTransactionPostData), ApiWallet())
import           Cardano.Wallet.Api.Types.SchemaMetadata            (TxMetadataSchema(..))
import           Cardano.Mnemonic                                   (SomeMnemonic, MkSomeMnemonic(..))
import           Cardano.Wallet.Primitive.AddressDerivation         (WalletKey(digest, publicKey))
import           Cardano.Wallet.Primitive.AddressDerivation.Shelley (generateKeyFromSeed)
import           Cardano.Wallet.Primitive.Passphrase                (Passphrase (..), currentPassphraseScheme, preparePassphrase)
import           Cardano.Wallet.Primitive.Types                     (WalletId(WalletId))
import           Cardano.Wallet.Primitive.Types.Address             (AddressState(..))
import           Control.Concurrent                                 (threadDelay)
import           Control.FromSum                                    (fromEither)
import           Control.Lens                                       ((<&>), (^?), (^.))
import           Control.Monad                                      (void, unless)
import           Control.Monad.IO.Class                             (MonadIO(..))
import           Data.Aeson                                         (FromJSON(..), ToJSON(..), (.:), eitherDecode, withObject)
import           Data.Aeson.Lens                                    (key, AsPrimitive(_String))
import qualified Data.ByteString.Lazy                               as LB
import           Data.Map                                           (keys)
import           Data.Maybe                                         (mapMaybe)
import           Data.String                                        (IsString(..))
import           Data.Text                                          (Text)
import qualified Data.Text                                          as T
import           Data.Text.Class                                    (FromText(fromText))
import           Data.Void                                          (Void)
import           GHC.Generics                                       (Generic)
import           Ledger                                             (Address, CardanoTx (..), Params (..), PaymentPubKeyHash, StakePubKeyHash, TxOutRef, StakingCredential)
import           Ledger.Ada                                         (lovelaceValueOf)
import           Ledger.Constraints                                 (TxConstraints, ScriptLookups, mustPayToPubKeyAddress, mustPayToPubKey, mkTxWithParams)
import           Ledger.Typed.Scripts                               (ValidatorTypes(..))
import           Ledger.Tx                                          (getCardanoTxId)
import           Ledger.Tx.CardanoAPI                               (unspentOutputsTx)
import           Plutus.Contract.Wallet                             (export)
import           PlutusTx.IsData                                    (ToData, FromData)
import           Prelude                                            hiding (replicate)
import           Utils.Address                                      (bech32ToAddress, bech32ToKeyHashes)
import           Utils.Passphrase                                   (convertPassphrase)
import           Utils.Prelude                                      (replicate)
import qualified Utils.Servant                                      as Servant
import           Utils.Tx                                           (apiSerializedTxToCardanoTx, cardanoTxToSealedTx)

------------------------------------------- Restore-wallet -------------------------------------------

class (Monad m, MonadIO m) => HasWallet m where
    getRestoreWallet :: m RestoredWallet

data RestoredWallet = RestoredWallet
    { name             :: Text
    , mnemonicSentence :: SomeMnemonic
    , passphrase       :: Passphrase "user"
    } deriving (Show, Generic)

instance FromJSON RestoredWallet where
    parseJSON = withObject "Restore wallet" $ \v -> do
        let mkMnemonic = either (fail . show) pure . mkSomeMnemonic @'[ 24 ]
            mkPassphrase = Passphrase . fromString
        name                   <- v .: "name"
        mnemonicSentence       <- v .: "mnemonic_sentence" >>= mkMnemonic
        passphrase             <- v .: "passphrase"        <&> mkPassphrase
        pure RestoredWallet{..}

genWalletId :: SomeMnemonic -> Passphrase "user" -> WalletId
genWalletId mnemonic pp = WalletId $ digest $ publicKey rootXPrv
  where
    rootXPrv = generateKeyFromSeed (mnemonic, Nothing) pwdP
    pwdP = preparePassphrase currentPassphraseScheme pp

restoreWalletFromFile :: MonadIO m => FilePath -> m RestoredWallet
restoreWalletFromFile fp = liftIO $ LB.readFile fp >>= (\case
    Right rw -> pure rw
    Left err -> error err) . eitherDecode

-- Read restore-wallet JSON file and generate walletId from it
walletIdFromFile :: MonadIO m => FilePath -> m WalletId
walletIdFromFile fp = do
    RestoredWallet{..} <- restoreWalletFromFile fp
    pure $ genWalletId mnemonicSentence passphrase

getWalletId :: HasWallet m => m WalletId
getWalletId = do
    RestoredWallet{..} <- getRestoreWallet
    pure $ genWalletId mnemonicSentence passphrase

------------------------------------------- Wallet functions -------------------------------------------

getFromEndpoint :: Servant.Endpoint a
getFromEndpoint = Servant.getFromEndpointOnPort 8090

-- Important note: this function only takes first used addres from the list, 
-- while the one with the highest UTXO's sum on it may be preferred.
-- Maybe later we should add a check for the UTXO's sum on each used address and select one with the maximum amount.
getWalletAddrBech32 :: HasWallet m => m Text
getWalletAddrBech32 = do
    walletId <- getWalletId
    getFromEndpoint (Client.listAddresses  Client.addressClient (ApiT walletId) (Just $ ApiT Used)) >>= \case
        v:_ -> pure $ v ^. key "id"._String
        _   -> error $  "There is no addresses associated with this wallet ID:\n" <> show walletId

getWalletAddr :: HasWallet m => m Address
getWalletAddr = do
    addrWalletBech32 <- getWalletAddrBech32
    pure $ case bech32ToAddress <$> fromText addrWalletBech32 of
        Right (Just addr) -> addr
        _                 -> error $ "Can't get wallet address from bech32: " <> T.unpack addrWalletBech32

getWalletKeyHashes :: HasWallet m => m (PaymentPubKeyHash, Maybe StakePubKeyHash)
getWalletKeyHashes = do
    addrWalletBech32 <- getWalletAddrBech32
    pure $ case bech32ToKeyHashes <$> fromText addrWalletBech32 of
        Right (Just hashes) -> hashes
        _                   -> error $ "Can't get wallet key hashes from bech32: " <> T.unpack addrWalletBech32

getWalletFromId :: HasWallet m => WalletId -> m ApiWallet
getWalletFromId = getFromEndpoint . Client.getWallet Client.walletClient . ApiT

ownAddresses :: HasWallet m => m [Address]
ownAddresses = mapMaybe bech32ToAddress <$> ownAddressesBech32

ownAddressesBech32 :: HasWallet m => m [Text]
ownAddressesBech32 = do
    walletId <- getWalletId
    as <- getFromEndpoint $ Client.listAddresses  Client.addressClient (ApiT walletId) Nothing
    pure $ map (^. key "id"._String) as

signTx :: HasWallet m => CardanoTx -> m CardanoTx
signTx (cardanoTxToSealedTx -> Just stx) = do
    ppUser   <- passphrase <$> getRestoreWallet
    walletId <- getWalletId
    let ppLenient = fromEither (error "Invalid passphrase.") $ convertPassphrase ppUser
    sign walletId ppLenient >>= (\case
        Just ctx -> pure ctx
        _        -> error "Unable to convert ApiSerialisedTransaction to a CardanoTx.") . apiSerializedTxToCardanoTx
    where
        sign walletId pp = getFromEndpoint $ Client.signTransaction Client.transactionClient
            (ApiT walletId)
            (ApiSignTransactionPostData (ApiT stx) (ApiT pp))
signTx _ = error "Unable to convert CardanoTx to a SealedTx."

balanceTx ::
    ( HasWallet m
    , FromData (DatumType a)
    , ToData (DatumType a)
    , ToData (RedeemerType a)
    ) =>
    Params -> ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> m CardanoTx
balanceTx params lookups cons = do
    walletId <- getWalletId
    balance walletId >>= (\case
        Just ctx -> pure ctx
        _        -> error "Unable to convert ApiSerialisedTransaction to a CardanoTx.") . apiSerializedTxToCardanoTx
    where
        -- tx to pass to the wallet as JSON
        etx = fromEither (error . show) $ export params $ fromEither (error . show) $ mkTxWithParams params lookups cons
        balance walletId = getFromEndpoint $ Client.balanceTransaction Client.transactionClient
            (ApiT walletId)
            (toJSON etx)

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: HasWallet m => CardanoTx -> m ()
submitTx (cardanoTxToSealedTx -> Just stx) = do
    walletId <- getWalletId
    void $ getFromEndpoint $
        Client.submitTransaction Client.transactionClient
            (ApiT walletId)
            (ApiSerialisedTransaction $ ApiT stx)
submitTx _ = error "Unable to convert CardanoTx to a SealedTx."

-- Send a balanced transaction to Cardano Wallet Backend and wait until transaction is confirmed or declined
submitTxConfirmed :: HasWallet m => CardanoTx -> m ()
submitTxConfirmed ctx = submitTx ctx >> awaitTxConfirmed ctx

-- Wait until a transaction is confirmed (added to the ledger).
-- If the transaction is never added to the ledger then 'awaitTxConfirmed' never
-- returns
awaitTxConfirmed :: HasWallet m => CardanoTx -> m ()
awaitTxConfirmed ctx = go
    where
        go = do
            walletId <- getWalletId
            res <- getFromEndpoint $ Client.getTransaction Client.transactionClient
                (ApiT walletId)
                (ApiTxId $ ApiT $ mkHash ctx)
                TxMetadataNoSchema
            unless (confirmedResponse res) $ liftIO (threadDelay 1_000_000) >> go
        confirmedResponse res = case res ^? key "status"._String of
            Just "in_ledger" -> True
            _                -> False
        mkHash = fromEither (error . show) . fromText . T.pack . show  . getCardanoTxId

-- Create and submit a transaction that produces a specific number of outputs at the target wallet address
getWalletTxOutRefs :: HasWallet m => Params -> PaymentPubKeyHash -> Maybe StakingCredential -> Integer -> m [TxOutRef]
getWalletTxOutRefs params pkh mbSkc n = do
    liftIO $ putStrLn "Balancing..."
    balancedTx <- balanceTx params lookups cons
    liftIO $ print balancedTx
    liftIO $ putStrLn "Signing..."
    signedTx <- signTx balancedTx
    liftIO $ print signedTx
    liftIO $ putStrLn "Submitting..."
    submitTxConfirmed signedTx
    let refs = case signedTx of
            EmulatorTx _    -> error "Can not get TxOutRef's from EmulatorTx."
            CardanoApiTx tx -> keys $ unspentOutputsTx tx
    liftIO $ putStrLn "Submitted!"
    return refs
    where
        lookups = mempty :: ScriptLookups Void
        cons    = case mbSkc of
            Just skc -> mconcat $ replicate n $ mustPayToPubKeyAddress pkh skc $ lovelaceValueOf 10_000_000
            Nothing -> mustPayToPubKey pkh $ lovelaceValueOf 10_000_000