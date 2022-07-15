{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module IO.Time where

import           Data.Time                         (nominalDiffTimeToSeconds)
import qualified Data.Time.Clock.POSIX             as Time
import           Ledger                            (POSIXTime (..))
import           PlutusTx.Prelude                  hiding ((<$>))
import           Prelude                           (IO, (<$>), floor)

currentTime :: IO POSIXTime
currentTime = f <$> Time.getPOSIXTime
    where
        f = POSIXTime . (* 1000) . floor . nominalDiffTimeToSeconds