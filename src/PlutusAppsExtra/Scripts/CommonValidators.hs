{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module PlutusAppsExtra.Scripts.CommonValidators where

import           Ledger.Typed.Scripts           (IsScriptContext (..), Language (..), Versioned (..))
import           Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import           Plutus.Script.Utils.V2.Scripts (validatorHash)
import           Plutus.V2.Ledger.Api
import           PlutusTx                       (applyCode, compile, liftCode)
import           PlutusTx.Prelude

{-# INLINABLE alwaysFalseValidatorCheck #-}
alwaysFalseValidatorCheck :: Integer -> () -> () -> ScriptContext -> Bool
alwaysFalseValidatorCheck _ _ _ _ = False

alwaysFalseValidator :: Integer -> Validator
alwaysFalseValidator salt = mkValidatorScript $
  $$(PlutusTx.compile [|| mkUntypedValidator . alwaysFalseValidatorCheck ||])
    `PlutusTx.applyCode`
            PlutusTx.liftCode salt

alwaysFalseValidatorV :: Integer -> Versioned Validator
alwaysFalseValidatorV = flip Versioned PlutusV2 . alwaysFalseValidator

alwaysFalseValidatorHash :: Integer -> ValidatorHash
alwaysFalseValidatorHash = validatorHash . alwaysFalseValidator

alwaysFalseValidatorAddress :: Integer -> Address
alwaysFalseValidatorAddress = mkValidatorAddress . alwaysFalseValidator