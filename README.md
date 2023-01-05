# Plutus-Apps-Extra

This library extends [plutus-apps](https://github.com/input-output-hk/plutus-apps) functionality by adding many new utility functions and types. It also includes our domain-specific language (DSL) for constructing Cardano transactions.

# ENCOINS transactions DSL

Module `Constraints.OffChain` contains the functions available in this DSL. These functions wrap the respective functions from [plutus-apps](https://github.com/input-output-hk/plutus-apps), resulting in cleaner and more concise code. To construct a complex transaction, you can use those with a `do` notation.

For example, the following function mints the specified tokens, consumes the given `TxOutRef`, and sends the tokens to a particular script address:
```haskell
myToken :: Value

myValidatorHash :: ValidatorHash

myMintingPolicy :: MintingPolicy

myTokensMintTx :: TxOutRef -> Integer -> TransactionBuilder ()
myTokensMintTx ref amt = do
    let v = scale amt myToken
    _ <- utxoSpentPublicKeyTx (\r _ -> ref == r)
    utxoProducedScriptTx myValidatorHash Nothing (v + adaValueOf 2) ()
    tokensMintedTx myMintingPolicy () v
```

You can then get the lookups and transaction constraints using `constructTx`:
```haskell
constructTx :: TransactionBuilder () -> Transaction ->
    Maybe (ScriptLookups Any, TxConstraints (RedeemerType Any) (DatumType Any))
constructTx builder tx = txConstructorResult $ builder `execState` tx
```