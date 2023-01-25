{-# LANGUAGE OverloadedStrings #-}

module Tests.Kupo where

import           Data.Text        (Text)
import qualified IO.ChainIndex as CI
import           IO.Kupo          (getUtxosAt)
import           Utils.Address    (bech32ToAddress)

test :: Text -> IO ()
test bech32 = do
    let Just addr = bech32ToAddress bech32
    res <- getUtxosAt addr
    res' <- CI.getUtxosAt addr
    putStrLn "\nKupo:\n"
    mapM_ print res
    putStrLn "\nChainIndex:\n"
    mapM_ print res'