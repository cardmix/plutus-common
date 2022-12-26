{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Tests.ChainIndex where

import IO.ChainIndex
import Utils.Address
import Data.Text (Text)
import qualified Data.Map as Map
import Ledger.Tx

testAllAddr :: IO ()
testAllAddr = do
    putStrLn "test1"
    test1
    putStrLn "test2"
    test2
    putStrLn "test3"
    test3
    putStrLn "test4"
    test4
    putStrLn "test5"
    test5
    putStrLn "test6"
    test6
    putStrLn "test7"
    test7
    putStrLn "byron test 1"
    testByronAddr1
    putStrLn "byron test 2"
    testByronAddr2
    putStrLn "contract test 1"
    testContract1
    putStrLn "contract test 2"
    testContract2

withTest :: Text -> IO ()
withTest addr = do
    let Just a = bech32ToAddress addr
    putStrLn "\nADDR:"
    print a
    utxos <- getUtxosAt a
    putStrLn "\nUTXOS:"
    print utxos
    mapM_ print utxos

-- Doesn't fails
test1 :: IO ()
test1 = withTest "addr_test1qq6kx34evye9n58a2zs7pl42mdaukvz9c9dp4vwaeqm3n9k4zm80fxvyqnr0pdvvrklrl5tjf8n6mklutg3ldvtp2c3s5ajkx4"

-- Doesn't fails
test2 :: IO ()
test2 = withTest "addr_test1qzq92mxzllakzsdfcdmpmy9596lsvg3ecszuyc583r3txf2svne4du2cl756dqldymql5p97ldurvhvfum6xczqr2rfsrfsxmt"

-- Doesn't fails 
test3 :: IO ()
test3 = withTest "addr_test1qpjegad0xrm0g3uyvc2despfdq2qk2adhmjs3rl2u0rfrej2ugpwwh9mnwh78xwnd32u8as09zq43we4230ncet8249sr589z5"

-- Doesn't fails
test4 :: IO ()
test4 = withTest "addr_test1qpmtp5t0t5y6cqkaz7rfsyrx7mld77kpvksgkwm0p7en7qum7a589n30e80tclzrrnj8qr4qvzj6al0vpgtnmrkkksnqd8upj0"

-- Doesn't fails
test5 :: IO ()
test5 = withTest "addr_test1qz8hpy5cz88gc92tsssq2454maj6e3pwmd52kym7pvln30tsxye7ww4lmawk7ynfq75dhy9n0kflkj4cy4s3c75vt69sasksrp"

-- Doesn't fails
test6 :: IO ()
test6 = withTest "addr_test1qzc62f70pn5l9aytwdwpnzfn0tyc9jxlar07nr4332vla7ms347sjjelw3e22se5lrnw968mnyvz5ma5hshl8lywv45qnmkvkl"

-- Doesn't fails
test7 :: IO ()
test7 = withTest "addr_test1qqzns5nk669jc9pdwnwcc69ysksmgdqc2uj99hza7yx5s0nun2906vgcxhvmjj44dvsg8v2fnulryx58xxefxlarfasqqv77y3"

testSum :: Text -> IO ()
testSum addr = do
    let Just a = bech32ToAddress addr
    putStrLn "\nADDR:"
    print a
    citxouts <- map snd . Map.toList <$> getUtxosAt a
    putStrLn "\nSUM:"
    print $ mconcat $ map _decoratedTxOutValue citxouts

-- Shows 2 less ada (514.712073 instead of 516.056871), but all tokens are shown correctly
testSum1 :: IO ()
testSum1 = testSum "addr_test1qzq92mxzllakzsdfcdmpmy9596lsvg3ecszuyc583r3txf2svne4du2cl756dqldymql5p97ldurvhvfum6xczqr2rfsrfsxmt"

-- Shows correct amount of ada
testSum2 :: IO ()
testSum2 = testSum "addr_test1qz9vf05xvsj6704l3wam2sk7mhdu83uacdzl7xf9yj93jztye3y03y730kx6gz52sar3yqw0zdxz32k5tak94spz07fqvugl3j"

-- Old daedalus-style address from Byron era
testByronAddr1 :: IO ()
testByronAddr1 = withTest "DdzFFzCqrhtArEct1dyJ75prgLDpo9zdkpGgy9Z1E7PJaZTwrbTCCxmAhFiUqSynYRTRfYThybHzmHbTJJ7xD6uEJoyXq3xqgZA35zp5"

-- Another Byron era address
testByronAddr2 :: IO ()
testByronAddr2 = withTest "37btjrVyb4KDXBNC4haBVPCrro8AQPHwvCMp3RFhhSVWwfFmZ6wwzSK6JK1hY6wHNmtrpTf1kdbva8TCneM2YsiXT7mrzT21EacHnPpz5YyUdj64na"

-- Contract addresses
testContract1 :: IO ()
testContract1 = withTest "addr1zxj47sy4qxlktqzmkrw8dahe46gtv8seakrshsqz26qnvzypw288a4x0xf8pxgcntelxmyclq83s0ykeehchz2wtspksr3q9nx"

testContract2 :: IO ()
testContract2 = withTest "addr1wxjwdkrr0ctw3dq9qm5vcr0jgd9n8k82y57ehurmfgnw5cgrxh0hr"

-- Fails with "Exception: DecodeFailure "Error in $: parsing Ledger.Tx.ChainIndexTxOut(ScriptChainIndexTxOut) failed, key \"_ciTxOutScriptDatum\" not found"..."
testFailedDatum :: IO ()
testFailedDatum = withTest "addr1w96dh8r6yds9et9wfmnrtg267mnrck3ej9wysyj5a6uhtzchkcr5d"