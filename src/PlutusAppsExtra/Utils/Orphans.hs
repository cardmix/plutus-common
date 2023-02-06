{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans               #-}

module PlutusAppsExtra.Utils.Orphans where

import           Ledger                    (PubKeyHash (..), StakePubKeyHash, ValidatorHash (..))
import           Ledger.Address            (Address (..), PaymentPubKeyHash (..), StakePubKeyHash (..))
import           Ledger.Tx                 (TxId (..), TxOutRef (..))
import           Plutus.V2.Ledger.Api      (Credential (..), StakingCredential (..))
import           PlutusTx.Prelude          hiding ((<$>), (<>))
import           Prelude                   ((<$>), (^))
import           Test.QuickCheck           (Arbitrary (..))

import           PlutusTx.Extra.ByteString (ToBuiltinByteString (..))

------------------------------------- Arbitrary --------------------------------------

instance Arbitrary TxOutRef where
    arbitrary = do
        bs <- arbitrary
        TxOutRef (TxId $ toBytes $ modulo bs (2 ^ (256 :: Integer) - 1)) . max 0 <$> arbitrary

instance Arbitrary PubKeyHash where
    arbitrary = do
        bs <- arbitrary
        return $ PubKeyHash $ toBytes $ modulo bs (2 ^ (256 :: Integer) - 1)

instance Arbitrary PaymentPubKeyHash where
    arbitrary = PaymentPubKeyHash <$> arbitrary

instance Arbitrary StakePubKeyHash where
    arbitrary = StakePubKeyHash <$> arbitrary

----------------------------------- ToBuiltinByteString -------------------------------

instance ToBuiltinByteString Credential where
    {-# INLINABLE toBytes #-}
    toBytes (PubKeyCredential (PubKeyHash bs)) = bs
    toBytes (ScriptCredential (ValidatorHash bs)) = bs

instance ToBuiltinByteString Address where
    {-# INLINABLE toBytes #-}
    toBytes (Address cred1 sCred) = case sCred of
        Just (StakingHash cred2) -> toBytes cred1 `appendByteString` toBytes cred2
        Just (StakingPtr i1 i2 i3) -> toBytes cred1 `appendByteString` toBytes i1
            `appendByteString` toBytes i2 `appendByteString` toBytes i3
        Nothing -> toBytes cred1

instance ToBuiltinByteString PubKeyHash where
    {-# INLINABLE toBytes #-}
    toBytes (PubKeyHash bs) = bs

instance ToBuiltinByteString PaymentPubKeyHash where
    {-# INLINABLE toBytes #-}
    toBytes (PaymentPubKeyHash (PubKeyHash bs)) = bs