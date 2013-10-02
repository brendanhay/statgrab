{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Module      : System.Statgrab
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Monadic context and data types for managing the underlying libstatgrab FFI calls
-- with transparent resource allocation and deallocation.
module System.Statgrab
    (
    -- * Operations on the @Stats@ Monad
      Stats
    , runStats
    , inspect

    -- * Concurrency
    , async

    -- * Statistics
    , Host             (..)
    , CPU              (..)
    , CPUPercent       (..)
    , Memory           (..)
    , Load             (..)
    , User             (..)
    , Swap             (..)
    , FileSystem       (..)
    , DiskIO           (..)
    , NetworkIO        (..)
    , NetworkInterface (..)
    , Page             (..)
    , Process          (..)
    , ProcessCount     (..)

    -- * Enums
    , HostState        (..)
    , CPUPercentSource (..)
    , DeviceType       (..)
    , InterfaceMode    (..)
    , InterfaceStatus  (..)
    , ProcessState     (..)
    , ProcessSource    (..)

    -- * Re-exported
    , Async
    , wait
    ) where

import           Control.Applicative
import           Control.Concurrent.Async   (Async, wait)
import qualified Control.Concurrent.Async   as Async
import qualified Control.Exception          as E
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import           GHC.Word
import           System.Statgrab.Base

newtype Stats a = Stats { unwrap :: ReaderT (IORef Word) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadCatchIO)

-- | Run the 'Stats' Monad, bracketing libstatgrab's sg_init and sg_shutdown
-- calls via reference counting.
runStats :: MonadCatchIO m => Stats a -> m a
runStats = liftIO
    . bracket (sg_init 0 >> newIORef 1) destroy
    . runReaderT
    . unwrap

-- | Run the 'Stats' Monad asynchronously. 'wait' from the async package can
-- be used to block and retrieve the result of the asynchronous computation.
async :: Stats a -> Stats (Async a)
async (Stats s) = Stats $ do
    ref <- ask
    liftIO $ do
        atomicModifyIORef' ref $ \ n -> (succ n, ())
        Async.async $ runReaderT s ref `E.finally` destroy ref

-- | Retrieve statistics from the underlying operating system. Please see the
-- exported data types for a list of available statistics.
inspect :: Info a => Stats a
inspect = liftIO $ acquire >>= copy

destroy :: IORef Word -> IO ()
destroy ref = do
    n <- atomicModifyIORef' ref $ \n -> (pred n, n)
    when (n == 1) $ void sg_shutdown
