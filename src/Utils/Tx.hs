{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Utils.Tx where

import           Cardano.Api.Shelley               (EraInMode (..), AsType (..), SerialiseAsCBOR (..), InAnyCardanoEra (..),
                                                    ConsensusMode (..), AnyCardanoEra (..), CardanoEra (..), toEraInMode)
import           Cardano.Wallet.Api.Types          (ApiSerialisedTransaction(..), getApiT)
import           Cardano.Wallet.Primitive.Types.Tx (SealedTx, sealedTxFromCardano', cardanoTxIdeallyNoLaterThan)
import           Control.FromSum                   (eitherToMaybe)
import           Data.Aeson.Extras                 (encodeByteString, tryDecode)
import           Data.Text                         (Text)
import           Ledger                            (Params)
import           Ledger.Constraints                (UnbalancedTx)
import           Ledger.Tx                         (CardanoTx (..), SomeCardanoApiTx (..))
import           Plutus.Contract.Wallet            (ExportTx (..), export)
import           PlutusTx.Prelude                  hiding ((<>))

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