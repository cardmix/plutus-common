{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Utils.Tx where

import           Cardano.Api.Shelley                 (AnyCardanoEra (..), AsType (..), CardanoEra (..), ConsensusMode (..),
                                                      EraInMode (..), InAnyCardanoEra (..), SerialiseAsCBOR (..), Tx (..),
                                                      toEraInMode)
import qualified Cardano.Crypto.DSIGN                as Crypto
import qualified Cardano.Ledger.Alonzo.TxWitness     as Wits
import           Cardano.Ledger.Babbage.Tx           (ValidatedTx (ValidatedTx))
import           Cardano.Ledger.Shelley.API          (VKey (VKey), WitVKey (WitVKey))
import           Cardano.Node.Emulator.Params        (Params)
import           Cardano.Wallet.Api.Types            (ApiSerialisedTransaction (..), getApiT)
import           Cardano.Wallet.LocalClient.ExportTx (ExportTx (..), export)
import           Cardano.Wallet.Primitive.Types.Tx   (SealedTx, cardanoTxIdeallyNoLaterThan, sealedTxFromCardano')
import           Control.FromSum                     (eitherToMaybe, fromMaybe)
import           Control.Lens                        (At (at), (&), (?~))
import           Data.Aeson.Extras                   (encodeByteString, tryDecode)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import           Ledger                              (PubKey (..), Signature (..), cardanoTxMap, signatures)
import           Ledger.Constraints                  (UnbalancedTx)
import           Ledger.Tx                           (CardanoTx (..), SomeCardanoApiTx (..))
import           Plutus.V1.Ledger.Bytes              (bytes, fromBytes)
import           Plutus.V2.Ledger.Api                (fromBuiltin, toBuiltin)
import           Text.Hex                            (decodeHex)

------------------------ Export/Import of transactions -------------------------

unbalancedTxToCBOR :: Params -> UnbalancedTx -> Maybe Text
unbalancedTxToCBOR params = 
    fmap (encodeByteString . serialiseToCBOR . partialTx) . eitherToMaybe . export params

textToCardanoTx :: Text -> Maybe CardanoTx
textToCardanoTx txt = do
    bs <- eitherToMaybe $ tryDecode txt
    tx <- eitherToMaybe $ deserialiseFromCBOR (AsTx AsBabbageEra) bs
    return $ CardanoApiTx $ SomeTx tx BabbageEraInCardanoMode

cardanoTxToText :: CardanoTx -> Maybe Text
cardanoTxToText (CardanoApiTx (SomeTx tx BabbageEraInCardanoMode)) = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText _ = Nothing

apiSerializedTxToCardanoTx :: ApiSerialisedTransaction -> Maybe CardanoTx
apiSerializedTxToCardanoTx = fmap CardanoApiTx . toSomeTx . toAnyEraTx
    where
        toAnyEraTx = cardanoTxIdeallyNoLaterThan (AnyCardanoEra BabbageEra) . getApiT . transaction
        toSomeTx (InAnyCardanoEra cera tx) = SomeTx tx <$> toEraInMode cera CardanoMode

cardanoTxToSealedTx :: CardanoTx -> Maybe SealedTx
cardanoTxToSealedTx = \case
    (CardanoApiTx (SomeTx tx _)) -> Just $ sealedTxFromCardano' tx
    _                            -> Nothing

------------------------ External keys and signatures -------------------------

textToPubkey :: Text -> Maybe PubKey
textToPubkey txt = PubKey . fromBytes <$> decodeHex txt

textToSignature :: Text -> Maybe Signature
textToSignature txt = Signature . toBuiltin <$> decodeHex txt

addCardanoTxSignature :: PubKey -> Signature -> CardanoTx -> CardanoTx
addCardanoTxSignature pubKey sig = cardanoTxMap addSignatureTx addSignatureCardano
    where
        addSignatureTx tx = tx & signatures . at pubKey ?~ sig 

        addSignatureCardano (CardanoApiEmulatorEraTx ctx)
            = CardanoApiEmulatorEraTx (addSignatureCardano' ctx)

        addSignatureCardano' (ShelleyTx shelleyBasedEra (ValidatedTx body wits isValid aux)) = 
            let wits' = wits <> mempty { Wits.txwitsVKey = Set.singleton $ WitVKey (VKey vk) sig' }
            in  ShelleyTx shelleyBasedEra (ValidatedTx body wits' isValid aux)
            
        vk = fromMaybe (error "addCardanoTxSignature: deserialise VerKeyDSIGN from a PubKey.")
            . Crypto.rawDeserialiseVerKeyDSIGN
            . bytes
            $ getPubKey pubKey

        sig' = Crypto.SignedDSIGN 
            . fromMaybe (error "addCardanoTxSignature: deserialise SigDSIGN from a Signature.")
            . Crypto.rawDeserialiseSigDSIGN 
            . fromBuiltin
            $ getSignature sig