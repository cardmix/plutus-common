{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module PlutusAppsExtra.IO.Time where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Time              (nominalDiffTimeToSeconds)
import qualified Data.Time.Clock.POSIX  as Time
import           Ledger                 (POSIXTime (..))
import           PlutusTx.Prelude       hiding ((<$>))
import           Prelude                (floor, (<$>))

class HasClock m where
    currentTime :: m POSIXTime

instance MonadIO m => HasClock m where
    currentTime = liftIO $ f <$> Time.getPOSIXTime
        where
            f = POSIXTime . (* 1000) . floor . nominalDiffTimeToSeconds