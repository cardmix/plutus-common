{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans            #-}

module Utils.Orphans where

import           Ledger.Tx                         (TxOutRef (..), TxId (..))
import           PlutusTx.Prelude                  hiding ((<$>), (<>))
import           Prelude                           ((<$>), (^))
import           Test.QuickCheck                   (Arbitrary (..))

import           Utils.ByteString                  (ToBuiltinByteString(..))

instance Arbitrary TxOutRef where
    arbitrary = do
        bs <- arbitrary
        TxOutRef (TxId $ toBytes $ modulo bs (2 ^ (256 :: Integer) - 1)) <$> arbitrary