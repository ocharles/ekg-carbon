{-# LANGUAGE OverloadedStrings #-}

-- | This module lets you periodically flush metrics to a Graphite Carbon
-- backend. Example usage:
--
-- > main = do
-- >   store <- newStore
-- >   forkCarbon defaultCarbonOptions store
--
-- You probably want to include some of the predefined metrics defined
-- in the @ekg-core@ package, by calling e.g. the 'EKG.registerGcMetrics'
-- function defined in that package.
module System.Remote.Monitoring.Carbon
  ( CarbonOptions(..)
  , defaultCarbonOptions
  , forkCarbon
  ) where

import Control.Concurrent (ThreadId, forkFinally, myThreadId, threadDelay, throwTo)
import Control.Monad (forever)
import Data.Int (Int64)
import Data.Monoid ((<>))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Vector as V
import qualified Network.Carbon.Plaintext as Carbon
import qualified Network.Socket as Network
import qualified System.Metrics as EKG
import qualified System.Metrics.Distribution as Stats

--------------------------------------------------------------------------------
-- | Options to control how to connect to the Carbon server and how often to
-- flush metrics. The flush interval should match the shortest retention rate
-- of the matching retention periods, or you risk over-riding previous
-- samples.
data CarbonOptions = CarbonOptions
  { -- | The hostname or IP address of the server running Carbon.
    host :: !T.Text

    -- | Server port of the TCP line receiver interface.
  , port :: !Int

    -- | The amount of time between sampling EKG metrics and pushing to Carbon.
  , flushInterval :: !Int

    -- | Prefix to add to all metric names.
  , prefix :: !T.Text

    -- | Suffix to add to all metric names. This is particularly
    -- useful for sending per host stats by settings this value to:
    -- @takeWhile (/= \'.\') \<$\> getHostName@, using @getHostName@
    -- from the @Network.BSD@ module in the network package.
  , suffix :: !T.Text
  } deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Defaults:
--
-- * @host@ = @\"127.0.0.1\"@
--
-- * @port@ = @2003@
--
-- * @flushInterval@ = @1000@
--
-- * Empty 'prefix' and 'suffix'.
defaultCarbonOptions :: CarbonOptions
defaultCarbonOptions = CarbonOptions
    { host          = "127.0.0.1"
    , port          = 2003
    , flushInterval = 1000
    , prefix        = ""
    , suffix        = ""
    }


--------------------------------------------------------------------------------
-- | Create a thread that periodically flushes the metrics in 'EKG.Store' to
-- Carbon.
forkCarbon :: CarbonOptions -> EKG.Store -> IO (ThreadId)
forkCarbon opts store = do
  addrInfos <- Network.getAddrInfo Nothing
                                   (Just $ T.unpack $ host opts)
                                   (Just $ show $ port opts)
  c <- case addrInfos of
    (addrInfo : _) -> Carbon.connect (Network.addrAddress addrInfo)
    _ -> unsupportedAddressError

  parent <- myThreadId
  forkFinally (loop store c opts)
              (\r -> do Carbon.disconnect c
                        case r of
                          Left e  -> throwTo parent e
                          Right _ -> return ())

  where
  unsupportedAddressError = ioError $ userError $
      "unsupported address: " ++ T.unpack (host opts)


--------------------------------------------------------------------------------
loop :: EKG.Store -> Carbon.Connection -> CarbonOptions -> IO ()
loop store socket opts = forever $ do
  start <- time
  sample <- EKG.sampleAll store
  flushSample sample socket opts
  end <- time
  threadDelay (flushInterval opts * 1000 - fromIntegral (end - start))

-- | Microseconds since epoch.
time :: IO Int64
time = (round . (* 1000000.0) . toDouble) `fmap` Time.getPOSIXTime
  where toDouble = realToFrac :: Real a => a -> Double

flushSample :: EKG.Sample -> Carbon.Connection -> CarbonOptions -> IO ()
flushSample sample conn opts = do
  t <- Time.getCurrentTime
  Carbon.sendMetrics conn
                     (V.map renamed
                            (HashMap.foldlWithKey' (\ms k v -> metrics k v t <> ms)
                                                   V.empty
                                                   sample))

  where
  renamed (Carbon.Metric n v t) =
    let p = if T.null (prefix opts) then "" else prefix opts <> "."
        s = if T.null (suffix opts) then "" else "." <> suffix opts
    in Carbon.Metric (p <> n <> s) v t

  metrics n (EKG.Counter i) t = V.singleton (Carbon.Metric n (fromIntegral i) t)
  metrics n (EKG.Gauge i) t = V.singleton (Carbon.Metric n (fromIntegral i) t)
  metrics _ (EKG.Label {}) _ = V.empty
  metrics n (EKG.Distribution stats) t =
    let f n' v = Carbon.Metric (n <> "." <> n') v t
    in V.fromList [ f "mean" (Stats.mean stats)
                  , f "variance" (Stats.variance stats)
                  , f "count" (fromIntegral $ Stats.count stats)
                  , f "sum" (Stats.sum stats)
                  , f "min" (Stats.min stats)
                  , f "max" (Stats.max stats)
                  ]
