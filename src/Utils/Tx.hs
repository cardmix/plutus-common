{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Utils.Tx where

import           Cardano.Api.Shelley     (EraInMode (..), AsType (..), SerialiseAsCBOR (..))
import           Data.Aeson.Extras       (encodeByteString, tryDecode)
import           Data.Text               (Text)
import           Ledger                  (Params)
import           Ledger.Constraints      (UnbalancedTx)
import           Ledger.Tx               (CardanoTx (..), SomeCardanoApiTx (..))
import           Plutus.Contract.Wallet  (ExportTx (..), export)
import           PlutusTx.Prelude        hiding ((<>))

------------------------ Export/Import of transactions -------------------------

unbalancedTxToCBOR :: Params -> UnbalancedTx -> Maybe Text
unbalancedTxToCBOR params = fmap (encodeByteString . serialiseToCBOR . partialTx) .
    either (const Nothing) Just . export params

textToCardanoTx :: Text -> Maybe CardanoTx
textToCardanoTx txt = do
    bs <- either (const Nothing) Just $ tryDecode txt
    tx <- either (const Nothing) Just $ deserialiseFromCBOR AsAlonzoTx bs
    return $ CardanoApiTx $ SomeTx tx AlonzoEraInCardanoMode

cardanoTxToText :: CardanoTx -> Maybe Text
cardanoTxToText (CardanoApiTx (SomeTx tx AlonzoEraInCardanoMode)) = Just $ encodeByteString $ serialiseToCBOR tx
cardanoTxToText _ = Nothing