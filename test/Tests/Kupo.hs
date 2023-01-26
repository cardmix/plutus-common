{-# LANGUAGE OverloadedStrings #-}

module Tests.Kupo where

import           Data.Function        (on)
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified IO.ChainIndex        as CI
import qualified IO.Kupo              as Kupo
import           Ledger               (DecoratedTxOut(..), TxId(..), TxOutRef(..))  
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
    putStrLn "the scripts are the same:"
    print $ ((==) `on` _decoratedTxOutReferenceScript) res res'

getScriptByHash :: IO ()
getScriptByHash = Kupo.getSciptByHash "a258f896dff1d01ac9a8bd0598304b933a8f3e9e0953938767178099" >>= print

ref :: TxOutRef
ref = TxOutRef (TxId $ toBuiltin $ fromJust $ T.decodeHex "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16") 1
