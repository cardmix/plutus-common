{-# LANGUAGE OverloadedStrings #-}

module Tests.Kupo where

import           Data.Function                 (on)
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import           Ledger                        (Address, DecoratedTxOut (..), TxId (..), TxOutRef (..))
import           Plutus.V1.Ledger.Api          (toBuiltin)
import qualified PlutusAppsExtra.IO.ChainIndex as CI
import qualified PlutusAppsExtra.IO.Kupo       as Kupo
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import qualified Text.Hex                      as T

getUtxosAt :: Text -> IO ()
getUtxosAt bech32 = do
    let Just addr = bech32ToAddress bech32
    res' <- CI.getUtxosAt addr
    putStrLn "\nChainIndex:\n"
    mapM_ print res'
    res <- Kupo.getUtxosAt addr
    putStrLn "\nKupo:\n"
    mapM_ print res
    putStrLn "\nSame res:\n"
    print $ res == res'

unspentTxOutFromRef :: TxOutRef -> IO ()
unspentTxOutFromRef ref = do 
    res <- Kupo.unspentTxOutFromRef ref
    res' <- CI.unspentTxOutFromRef ref
    putStrLn "\nKupo:\n"
    print res
    putStrLn "\nChainIndex:\n"
    print res'
    putStrLn "\nSame res:\n"
    print $ res == res'
    

veryLongTest :: IO ()
veryLongTest = getUtxosAt "addr_test1wqr4uz0tp75fu8wrg6gm83t20aphuc9vt6n8kvu09ctkugq6ch8kj"

getScriptByHash :: IO ()
getScriptByHash = Kupo.getSciptByHash "a258f896dff1d01ac9a8bd0598304b933a8f3e9e0953938767178099" >>= print

addrContract :: Text
addrContract = "addr_test1wpadr7r28cnwfvgmjrn2784j2netmjd0net7t4dkwqt0wzscyypnc"

ref1 :: TxOutRef
ref1 = TxOutRef (TxId $ toBuiltin $ fromJust $ T.decodeHex "7db1a328bcf2251a59e2f4362057188b11bf4d768a5797fad2a92e65a4370c74") 1