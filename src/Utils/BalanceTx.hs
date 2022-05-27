{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Utils.BalanceTx where

import           Data.Map                          (Map, empty, fromList, filterWithKey, keys, elems)
import qualified Data.Map
import qualified Data.Set
import           Ledger                            (Value, Address, ChainIndexTxOut(..), TxOutRef(..), minAdaTxOut)
import           Ledger.Tx                         (Tx(..), TxOut(..), pubKeyTxIn, toTxOut)
import           Plutus.Contract                   (Contract, ContractError (..), logInfo, throwError)
import           Plutus.Contract.Request           (utxosAt)
import           Plutus.V1.Ledger.Ada              (toValue, lovelaceValueOf)
import           Plutus.V1.Ledger.Value            (geq)
import           Ledger.Constraints.OffChain       (UnbalancedTx(..))
import           PlutusTx.Prelude                  hiding ((<>))


selectUTXOWithValue :: Map TxOutRef ChainIndexTxOut -> Value -> (TxOutRef, Value, Map TxOutRef ChainIndexTxOut)
selectUTXOWithValue utxos val = (key, v, filterWithKey (\k _ -> k == key) utxos')
  where utxos' = Data.Map.filter (\o -> _ciTxOutValue o `geq` val) utxos
        key    = head $ keys utxos'
        v      = _ciTxOutValue $ head $ elems utxos'

addUTXOUntil :: Map TxOutRef ChainIndexTxOut -> Value -> [Value] -> Maybe (Value, Map TxOutRef ChainIndexTxOut)
addUTXOUntil utxos val fs = do
    utxos' <- Data.Map.foldrWithKey f (Just empty) utxos
    let change  = sum (map _ciTxOutValue $ Data.Map.elems utxos') - (val + fs !! length (Data.Map.elems utxos'))
    Just (change, utxos')
  where f k o (Just m) = do
          let n = length (Data.Map.elems m)
          actualFee <- if length fs < n - 1 then Nothing else Just $ fs !! n
          if sum (map _ciTxOutValue $ Data.Map.elems m) `geq` (val + actualFee + toValue minAdaTxOut) then Just m else Just $ Data.Map.insert k o m
        f _ _ Nothing = Nothing

removeCollateralUTXO :: Map TxOutRef ChainIndexTxOut -> Map TxOutRef ChainIndexTxOut
removeCollateralUTXO utxos = Data.Map.difference utxos col
  where cols = Data.Map.filter (\o -> lovelaceValueOf 10_000_000 `geq` _ciTxOutValue o) utxos
        col  = if not $ Data.Map.null cols then Data.Map.fromList [head $ Data.Map.toList cols] else Data.Map.empty

balanceTxWithExternalWallet :: UnbalancedTx -> (Address, Value) -> [Value] -> Contract w s ContractError UnbalancedTx
balanceTxWithExternalWallet utx (addr, val) vals = do
    utxos <- utxosAt addr
    logInfo utxos
    (change, utxos') <- case addUTXOUntil utxos val vals of
                          Nothing -> throwError $ OtherContractError "Cannot balance transaction!"
                          Just r  -> pure r -- We assume that val is equal to the difference between outputs and inputs plus the fee
    logInfo change
    let tx      = unBalancedTxTx utx
        ins     = txInputs tx `Data.Set.union` Data.Set.fromList (map pubKeyTxIn $ keys utxos')
        outs    = txOutputs tx ++ [TxOut addr change Nothing]
        tx'     = tx { txInputs = ins, txOutputs = outs }
        utx'    = utx { unBalancedTxTx = tx', unBalancedTxUtxoIndex = Data.Map.map toTxOut utxos' }
    return utx'
