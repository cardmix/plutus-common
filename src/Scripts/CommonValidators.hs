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

module Scripts.CommonValidators where

import           Ledger.Typed.Scripts                 (IsScriptContext(..), Versioned (..), Language (..))
import           Plutus.Script.Utils.V2.Address       (mkValidatorAddress)
import           Plutus.Script.Utils.V2.Scripts       (validatorHash)
import           Plutus.V2.Ledger.Api
import           PlutusTx                             (compile)
import           PlutusTx.Prelude

{-# INLINABLE alwaysFalseValidatorCheck #-}
alwaysFalseValidatorCheck :: () -> () -> ScriptContext -> Bool
alwaysFalseValidatorCheck _ _ _ = False

alwaysFalseValidator :: Validator
alwaysFalseValidator = mkValidatorScript
  $$(PlutusTx.compile [|| mkUntypedValidator alwaysFalseValidatorCheck ||])

alwaysFalseValidatorV :: Versioned Validator
alwaysFalseValidatorV = Versioned alwaysFalseValidator PlutusV2

alwaysFalseValidatorHash :: ValidatorHash
alwaysFalseValidatorHash = validatorHash alwaysFalseValidator

alwaysFalseValidatorAddress :: Address
alwaysFalseValidatorAddress = mkValidatorAddress alwaysFalseValidator