{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tests.Kupo where

import           Data.Text        (Text)
import qualified IO.ChainIndex    as CI
import qualified IO.Kupo          as Kupo
import           Ledger           (TxOutRef(..), TxId (TxId))
import           Utils.Address    (bech32ToAddress)
import Data.String (IsString(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Plutus.V1.Ledger.Api (toBuiltin)
import qualified Text.Hex as T
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BSL
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Data.Word (Word8)

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
