{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module IO.Test where

import IO.ChainIndex
import Utils.Address
import Data.Text (Text)
import qualified Data.Map as Map
import Ledger.Tx

withTest :: Text -> IO ()
withTest addr = do
    let Just a = bech32ToAddress addr
    putStrLn "\nADDR:"
    print a
    utxos <- getUtxosAt a
    putStrLn "\nUTXOS:"
    mapM_ print utxos

-- | Addres that failed with undefined from src/Ledger/Tx/CardanoAPI.hs:252:5 in 
-- plutus-ledger-1.0.0-481b82ac3e5b049b3da12e30b10a8656f8f8a84c34a510497de51a0ea767e6ae:Ledger.Tx.CardanoAPI
test1 :: IO ()
test1 = withTest "addr_test1qq6kx34evye9n58a2zs7pl42mdaukvz9c9dp4vwaeqm3n9k4zm80fxvyqnr0pdvvrklrl5tjf8n6mklutg3ldvtp2c3s5ajkx4"

-- | Addres that not fail
test2 :: IO ()
test2 = withTest "addr_test1qzq92mxzllakzsdfcdmpmy9596lsvg3ecszuyc583r3txf2svne4du2cl756dqldymql5p97ldurvhvfum6xczqr2rfsrfsxmt"

-- | Same fail as in test1
test3 :: IO ()
test3 = withTest "addr_test1qpjegad0xrm0g3uyvc2despfdq2qk2adhmjs3rl2u0rfrej2ugpwwh9mnwh78xwnd32u8as09zq43we4230ncet8249sr589z5"

-- | Failed while accessing first endpoint
test4 :: IO ()
test4 = withTest "addr_test1qpmtp5t0t5y6cqkaz7rfsyrx7mld77kpvksgkwm0p7en7qum7a589n30e80tclzrrnj8qr4qvzj6al0vpgtnmrkkksnqd8upj0"

-- | Same as in 3 and 1
test5 :: IO ()
test5 = withTest "addr_test1qz8hpy5cz88gc92tsssq2454maj6e3pwmd52kym7pvln30tsxye7ww4lmawk7ynfq75dhy9n0kflkj4cy4s3c75vt69sasksrp"

-- | Same
test6 :: IO ()
test6 = withTest "addr_test1qzc62f70pn5l9aytwdwpnzfn0tyc9jxlar07nr4332vla7ms347sjjelw3e22se5lrnw968mnyvz5ma5hshl8lywv45qnmkvkl"

test7 :: IO ()
test7 = withTest "addr_test1qqc73s00kdmeegm5fcd8k5l8w2z3m0ffx820n83sfr7ug9yknvyxgh4ryhpu962vgy6cgk7s367e30qfey6xet98mjmsz8w7pm"

testSum :: Text -> IO ()
testSum addr = do
    let Just a = bech32ToAddress addr
    citxouts <- map (fst . snd) . Map.toList <$> getUtxosAt a
    putStrLn "\nSUM:"
    print $ mconcat $ map _ciTxOutValue citxouts

testSum1 :: IO ()
testSum1 = testSum "addr_test1qzq92mxzllakzsdfcdmpmy9596lsvg3ecszuyc583r3txf2svne4du2cl756dqldymql5p97ldurvhvfum6xczqr2rfsrfsxmt"