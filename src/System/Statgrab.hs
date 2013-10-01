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

-- |
module System.Statgrab where

import           Control.Applicative
import           Control.Concurrent.Async   (Async)
import qualified Control.Concurrent.Async   as Async
import qualified Control.Exception          as E
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import           GHC.Word
import           System.Statgrab.Base
import           System.Statgrab.Types

newtype Stats a = Stats { unwrap :: ReaderT (IORef Word) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadCatchIO, MonadPlus)

instance Alternative Stats where
    empty = Stats mzero
    (<|>) = mplus

-- runStats :: MonadCatchIO m => Stats a -> m a
-- runStats = liftIO
--     . bracket (sg_init >> newIORef 1) destroy
--     . runReaderT
--     . unwrap

-- async :: Stats a -> Stats (Async a)
-- async (Stats s) = Stats $ do
--     ref <- ask
--     liftIO $ do
--         atomicModifyIORef' ref $ \ n -> (succ n, ())
--         Async.async $ runReaderT s ref `E.finally` destroy ref


-- --
-- -- Internal
-- --

-- destroy :: IORef Word -> IO ()
-- destroy ref = do
--     n <- atomicModifyIORef' ref $ \n -> (pred n, n)
--     when (n == 1) $ void sg_shutdown

-- -- host :: Stats Host

-- -- cpu :: Stats Cpu

-- -- -- cpuDiff

-- -- percentages :: Stats Percentages

-- -- memory

-- -- load

-- -- user

-- -- swap

-- -- disk

-- -- diskIO

-- -- -- diskIODiff

-- -- network

-- -- networkIO

-- -- -- networkIODiff

-- -- page

-- -- -- pageDiff

-- -- process

-- -- processCount
