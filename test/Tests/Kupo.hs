{-# LANGUAGE OverloadedStrings #-}

module Tests.Kupo where

import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified IO.ChainIndex        as CI
import qualified IO.Kupo              as Kupo
import           Ledger               (TxOutRef(..), TxId (TxId))
import           Plutus.V1.Ledger.Api (toBuiltin)
import qualified Text.Hex             as T
import           Utils.Address        (bech32ToAddress)

getUtxosAt :: Text -> IO ()
getUtxosAt bech32 = do
    let Just addr = bech32ToAddress bech32
    res <- Kupo.getUtxosAt addr
    res' <- CI.getUtxosAt addr
    putStrLn "\nKupo:\n"
    mapM_ print res
    putStrLn "\nChainIndex:\n"
    mapM_ print res'

unspentTxOutFromRef :: TxOutRef -> IO ()
unspentTxOutFromRef ref = do 
    res <- Kupo.unspentTxOutFromRef ref
    res' <- CI.unspentTxOutFromRef ref
    putStrLn "\nKupo:\n"
    print res
    putStrLn "\nChainIndex:\n"
    print res'

ref :: TxOutRef
ref = TxOutRef (TxId $ toBuiltin $ fromJust $ T.decodeHex "470abf22c0ded4f2c0a06a762d50cb3cb4cbbea3cd0eba6b1e4aafad82d46084") 4
