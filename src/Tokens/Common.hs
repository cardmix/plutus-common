{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Tokens.Common (
    tokensRequired,
    tokensBurned,
    tokensTx,
    tokensMintTx
) where

import qualified Data.Map
import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints.TxConstraints (TxConstraints, mustSpendAtLeast, mustMintCurrencyWithRedeemer)
import           Ledger.Constraints.OffChain      (ScriptLookups, unspentOutputs, mintingPolicy)
import           Ledger.Value                     (geq, assetClassValue)
import           Plutus.Contract                  (Contract)
import           Plutus.Contract.Types            (AsContractError)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Prelude                          ((<$>))

import           Utils.ChainIndex                 (getUtxosWithCurrency)

--------------------------- On-Chain -----------------------------

{-# INLINABLE tokensRequired #-}
tokensRequired :: Value -> ScriptContext -> Bool
tokensRequired v ctx = val `geq` v
    where info = scriptContextTxInfo ctx
          ins  = txInfoInputs info
          val  = sum (map (txOutValue . txInInfoResolved) ins)

{-# INLINABLE tokensBurned #-}
tokensBurned :: Value -> ScriptContext -> Bool
tokensBurned v ctx = vIns `geq` (vOuts + v)
    where info  = scriptContextTxInfo ctx
          ins   = txInfoInputs info
          vIns  = sum (map (txOutValue . txInInfoResolved) ins)
          vOuts = sum (map txOutValue (txInfoOutputs info))

-------------------------- Off-Chain -----------------------------

-- TxConstraints that the tokens are consumed by the transaction
tokensTx :: (AsContractError e) => AssetClass -> Integer -> Contract w s e (ScriptLookups a, TxConstraints i o)
tokensTx ac n = do
    utxos <- Data.Map.map fst <$> getUtxosWithCurrency ac
    return (unspentOutputs utxos, mustSpendAtLeast $ assetClassValue ac n)

-- TxConstraints that the tokens are minted in the transaction
tokensMintTx :: MintingPolicy -> Redeemer -> TokenName -> Integer -> (ScriptLookups a, TxConstraints i o)
tokensMintTx mp r tn n = (mintingPolicy mp, mustMintCurrencyWithRedeemer (mintingPolicyHash mp) r tn n)