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
import           Control.Monad                                      (void, unless, MonadFail (fail))
import           Data.Aeson                                         (FromJSON(..), ToJSON(..), (.:), eitherDecode, withObject)
import           Data.Aeson.Lens                                    (key, AsPrimitive(_String))
import qualified Data.ByteString.Lazy                               as LB
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
-- import           PlutusTx.Prelude                                   hiding (mempty, pure, (<$>), unless, error)
-- import           Prelude                                            (IO, mempty, (<$>), Show (..), print, Applicative (pure), error, FilePath, putStrLn)
import           Utils.Passphrase                                   (convertPassphrase)
import           Utils.Prelude                                      (replicate)
import qualified Utils.Servant                                      as Servant
import           Utils.Tx                                           (apiSerializedTxToCardanoTx, cardanoTxToSealedTx)

import Prelude hiding (replicate)

getWalletId :: IO WalletId
getWalletId = walletIdFromFile "testnet/wallet.json"

-- Important note: this function only takes first used addres from the list, 
-- while the one with the highest UTXO's sum on it may be preferred.
-- Maybe later we should add a check for the UTXO's sum on each used address and select one with the maximum amount.
getWalletAddr :: IO Text
getWalletAddr = do
    walletId <- getWalletId
    getFromEndpoint (Client.listAddresses  Client.addressClient (ApiT walletId) (Just $ ApiT Used)) >>= \case
        v:_ -> pure $ v ^. key "id"._String
        _   -> error $  "There is no addresses associated with this wallet ID:\n" <> show walletId 

getFromEndpoint :: Servant.Endpoint a
getFromEndpoint = Servant.getFromEndpointOnPort 8090

getWalletFromId :: WalletId -> IO ApiWallet
getWalletFromId = getFromEndpoint . Client.getWallet Client.walletClient . ApiT 

-- getKnownWalletId :: IO WalletId
-- getKnownWalletId = getApiT . Wallet.id <$> getKnownWallet

-- getKnownWallet :: IO ApiWallet
-- getKnownWallet = getFromEndpoint $ Client.listWallets Client.walletClient >>= \case
--     w:_ -> pure w
--     _   -> error "There is no any known wallets yet."

signTx :: CardanoTx -> IO CardanoTx
signTx (cardanoTxToSealedTx -> Just stx) = do
    (walletId, ppUser) <- walletIdAndPassphraseFromFile "testnet/wallet.json"
    let ppLenient = fromEither (error "Invalid passphrase.") $ convertPassphrase ppUser
    apiSerializedTxToCardanoTx <$> sign walletId ppLenient >>= \case
        Just ctx -> pure ctx
        _        -> error "Unable to convert ApiSerialisedTransaction to a CardanoTx."
    where
        sign walletId pp = getFromEndpoint $ Client.signTransaction Client.transactionClient
            (ApiT walletId)
            (ApiSignTransactionPostData (ApiT stx) (ApiT pp))
signTx _ = error "Unable to convert CardanoTx to a SealedTx."

balanceTx :: (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a)) =>
    Params -> ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> IO CardanoTx
balanceTx params lookups cons = do
    walletId <- getWalletId
    apiSerializedTxToCardanoTx <$> balance walletId >>= \case
        Just ctx -> pure ctx
        _        -> error "Unable to convert ApiSerialisedTransaction to a CardanoTx."
    where 
        -- tx to pass to the wallet as JSON
        etx = fromEither (error . show) $ export params $ fromEither (error . show) $ mkTx lookups cons
        balance walletId = getFromEndpoint $ Client.balanceTransaction Client.transactionClient
            (ApiT walletId)
            (toJSON etx)

-- Send a balanced transaction to Cardano Wallet Backend and return immediately
submitTx :: CardanoTx -> IO ()
submitTx (cardanoTxToSealedTx -> Just stx) = do
    walletId <- getWalletId
    void $ getFromEndpoint $ 
        Client.submitTransaction Client.transactionClient
            (ApiT walletId)
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
            walletId <- getWalletId
            res <- getFromEndpoint $ Client.getTransaction Client.transactionClient
                (ApiT walletId)
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
    putStrLn "Balancing..." 
    balancedTx <- balanceTx params lookups cons
    print balancedTx
    putStrLn "Signing..." 
    signedTx <- signTx balancedTx
    print signedTx
    putStrLn "Submitting..."
    submitTxConfirmed signedTx
    let refs = case signedTx of
            EmulatorTx _    -> error "Can not get TxOutRef's from EmulatorTx."
            CardanoApiTx tx -> keys $ unspentOutputsTx tx
            Both _ tx       -> keys $ unspentOutputsTx tx
    putStrLn "Submitted!"
    return refs
    where
        lookups = mempty :: ScriptLookups Void
        cons    = case mbSkh of
            Just skh -> mconcat $ replicate n $ mustPayToPubKeyAddress pkh skh $ lovelaceValueOf 10_000_000
            Nothing -> mustPayToPubKey pkh $ lovelaceValueOf 10_000_000

------------------------------------------- Restore-wallet -------------------------------------------

data RestoreWallet = RestoreWallet
    { name             :: Text
    , mnemonicSentence :: SomeMnemonic
    , passphrase       :: Passphrase "user"
    } deriving Generic

instance FromJSON RestoreWallet where
    parseJSON = withObject "Restore wallet" $ \v -> do 
        let mkMnemonic = either (fail . show) pure . mkSomeMnemonic @'[ 24 ]
            mkPassphrase = Passphrase . fromString
        name                   <- v .: "name"
        mnemonicSentence       <- v .: "mnemonic_sentence" >>= mkMnemonic
        passphrase             <- v .: "passphrase"        <&> mkPassphrase
        pure RestoreWallet{..}

genWalletId :: SomeMnemonic -> Passphrase "user" -> WalletId
genWalletId mnemonic pp = WalletId $ digest $ publicKey rootXPrv
  where
    rootXPrv = generateKeyFromSeed (mnemonic, Nothing) pwdP
    pwdP = preparePassphrase currentPassphraseScheme pp

-- Read restore-wallet JSON file and generate walletId from it
walletIdFromFile :: FilePath -> IO WalletId
walletIdFromFile fp = eitherDecode <$> LB.readFile fp >>= \case
    Right RestoreWallet{..} -> pure $ genWalletId mnemonicSentence passphrase
    Left err                -> error err

walletIdAndPassphraseFromFile :: FilePath -> IO (WalletId, Passphrase "user")
walletIdAndPassphraseFromFile fp = eitherDecode <$> LB.readFile fp >>= \case
    Right RestoreWallet{..} -> pure (genWalletId mnemonicSentence passphrase, passphrase)
    Left err                -> error err