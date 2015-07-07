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
  , forkCarbonRestart
  ) where

import Control.Exception (SomeException, try)
import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay, throwTo)
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
-- Carbon. If the thread flushing statistics throws an exception (for example, the
-- network connection is lost), this exception will be thrown up to the thread
-- that called 'forkCarbon'. For more control, see 'forkCarbonRestart'.
forkCarbon :: CarbonOptions -> EKG.Store -> IO ThreadId
forkCarbon opts store =
  do parent <- myThreadId
     forkCarbonRestart opts
                       store
                       (\e _ -> throwTo parent e)

--------------------------------------------------------------------------------
-- | Create a thread that periodically flushes the metrics in 'EKG.Store' to
-- Carbon. If the thread flushing statistics throws an exception (for example, the
-- network connection is lost), the callback function will be invoked with the
-- exception that was thrown, and an 'IO' computation to restart the handler.
--
-- For example, you can use 'forkCarbonRestart' to log failures and restart
-- logging:
--
-- @
-- 'forkCarbonRestart' opts
--                     store
--                     (\ex restart -> do hPutStrLn stderr ("ekg-carbon: " ++ show ex)
--                                        restart)
-- @
forkCarbonRestart :: CarbonOptions
                  -> EKG.Store
                  -> (SomeException -> IO () -> IO ())
                  -> IO ThreadId
forkCarbonRestart opts store exceptionHandler =
  do addrInfos <-
       Network.getAddrInfo Nothing
                           (Just (T.unpack (host opts)))
                           (Just (show (port opts)))
     addrInfo <-
       case addrInfos of
         (addrInfo:_) -> return $ Network.addrAddress addrInfo
         _ -> unsupportedAddressError
     let go =
           do
              terminated <- try $ do
                c <- Carbon.connect addrInfo
                loop store c opts
              case terminated of
                Left exception ->
                  exceptionHandler exception go
                Right _ -> go
     forkIO go
  where unsupportedAddressError =
          ioError (userError ("unsupported address: " ++ T.unpack (host opts)))


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
