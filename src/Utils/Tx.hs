{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Utils.Tx where

import           Cardano.Api.Shelley               (EraInMode (..), AsType (..), SerialiseAsCBOR (..), InAnyCardanoEra (..),
                                                    ConsensusMode (..), AnyCardanoEra (..), CardanoEra (..), toEraInMode, Tx (..))
import qualified Cardano.Crypto.DSIGN              as Crypto
import qualified Cardano.Ledger.Alonzo.TxWitness   as Wits
import           Cardano.Ledger.Babbage.Tx         (ValidatedTx(ValidatedTx))
import           Cardano.Ledger.Shelley.API        (WitVKey(WitVKey), VKey(VKey))
import           Cardano.Wallet.Api.Types          (ApiSerialisedTransaction(..), getApiT)
import           Cardano.Wallet.Primitive.Types.Tx (SealedTx, sealedTxFromCardano', cardanoTxIdeallyNoLaterThan)
import qualified Data.Set                          as Set
import           Control.FromSum                   (fromMaybe, eitherToMaybe, fromEither)                   
import           Control.Lens                      (At (at), (&), (?~))
import           Data.Aeson.Extras                 (encodeByteString, tryDecode)
import           Data.Text                         (Text)
import           Ledger                            (Params, Signature (..), cardanoTxMap, signatures, PubKey (..))
import           Ledger.Constraints                (UnbalancedTx)
import           Ledger.Tx                         (CardanoTx (..), SomeCardanoApiTx (..))
import           Plutus.V1.Ledger.Bytes            (fromHex)
import           Plutus.Contract.Wallet            (ExportTx (..), export)
import           Plutus.V2.Ledger.Api              (fromBuiltin)

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

addCardanoTxSignature :: Crypto.VerKeyDSIGN Crypto.Ed25519DSIGN -> Signature -> CardanoTx -> CardanoTx
addCardanoTxSignature vk sig = cardanoTxMap addSignatureTx addSignatureCardano
    where
        addSignatureTx tx = tx & signatures . at vkPub ?~ sig 

        addSignatureCardano (CardanoApiEmulatorEraTx ctx)
            = CardanoApiEmulatorEraTx (addSignatureCardano' ctx)

        addSignatureCardano' (ShelleyTx shelleyBasedEra (ValidatedTx body wits isValid aux)) = 
            let wits' = wits <> mempty { Wits.txwitsVKey = Set.singleton $ WitVKey (VKey vk) sig' }
            in  ShelleyTx shelleyBasedEra (ValidatedTx body wits' isValid aux)
            
        vkPub = PubKey
            . fromEither (error "addCardanoTxSignature: deserialise pubKey from VerKeyDSIGN") 
            . fromHex
            $ Crypto.rawSerialiseVerKeyDSIGN vk

        sig' = Crypto.SignedDSIGN 
            . fromMaybe (error "addCardanoTxSignature: byron and shelley signature sizes do not match")
            . Crypto.rawDeserialiseSigDSIGN 
            . fromBuiltin
            $ getSignature sig