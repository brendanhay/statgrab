{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Module      : System.Statgrab.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Interface types for copying and unmarshalling libstatgrab structs into a
-- more programmer friendly Haskell format.
module System.Statgrab.Internal where

import Control.Applicative
import Data.ByteString       (ByteString)
import Data.Time.Clock.POSIX
import Foreign
import Foreign.C.Types
import GHC.Generics          (Generic)

type Entries = Ptr CSize

newtype Error = Error CInt
    deriving
        ( Eq
        , Ord
        , Enum
        , Bounded
        , Integral
        , Num
        , Real
        , Show
        , Storable
        , Generic
        )

newtype HostState = HostState CInt
    deriving
        ( Eq
        , Ord
        , Enum
        , Bounded
        , Integral
        , Num
        , Real
        , Show
        , Storable
        , Generic
        )

data Host = Host
    { hostOsName    :: !ByteString
    , hostOsRelease :: !ByteString
    , hostOsVersion :: !ByteString
    , hostPlatform  :: !ByteString
    , hostName      :: !ByteString
    , hostBitWidth  :: !Integer
    , hostState     :: !HostState
    , hostNCPU      :: !Integer
    , hostMaxCPU    :: !Integer
    , hostUptime    :: !POSIXTime
    , hostSystime   :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

data CPU = CPU
    { cpuUser                   :: !Integer
    , cpuKernel                 :: !Integer
    , cpuIdle                   :: !Integer
    , cpuIOWait                 :: !Integer
    , cpuSwap                   :: !Integer
    , cpuNice                   :: !Integer
    , cpuTotal                  :: !Integer
    , cpuCtxSwitches            :: !Integer
    , cpuVoluntaryCtxSwitches   :: !Integer
    , cpuInvoluntaryCtxSwitches :: !Integer
    , cpuSyscalls               :: !Integer
    , cpuInterrupts             :: !Integer
    , cpuSoftInterrupts         :: !Integer
    , cpuSystime                :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

newtype CPUPercentSource = CPUPercentSource CInt
    deriving
        ( Eq
        , Ord
        , Enum
        , Bounded
        , Integral
        , Num
        , Real
        , Show
        , Storable
        , Generic
        )

data CPUPercent = CPUPercent
    { cpuPctUser      :: !Double
    , cpuPctKernel    :: !Double
    , cpuPctIdle      :: !Double
    , cpuPctIOWait    :: !Double
    , cpuPctSwap      :: !Double
    , cpuPctNice      :: !Double
    , cpuPctTimeTaken :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

data Memory = Memory
    { memTotal   :: !Integer
    , memFree    :: !Integer
    , memUsed    :: !Integer
    , memCache   :: !Integer
    , memSystime :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

data Load = Load
    { load1       :: !Double
    , load5       :: !Double
    , load15      :: !Double
    , loadSystime :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

data User = User
    { userLoginName    :: !ByteString
    , userRecordId     :: !ByteString
    , userRecordIdSize :: !Integer
    , userDevice       :: !ByteString
    , userHostName     :: !ByteString
    , userPid          :: !Integer
    , userLoginTime    :: !POSIXTime
    , userSystime      :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

data Swap = Swap
    { swapTotal   :: !Integer
    , swapUsed    :: !Integer
    , swapFree    :: !Integer
    , swapSystime :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

newtype DeviceType = DeviceType CInt
    deriving
        ( Eq
        , Ord
        , Enum
        , Bounded
        , Integral
        , Num
        , Real
        , Show
        , Storable
        , Generic
        )

data FileSystem = FileSystem
    { fsDeviceName  :: !ByteString
    , fsType        :: !ByteString
    , fsMountPoint  :: !ByteString
    , fsDeviceType  :: !DeviceType
    , fsSize        :: !Integer
    , fsUsed        :: !Integer
    , fsFree        :: !Integer
    , fsAvail       :: !Integer
    , fsTotalInodes :: !Integer
    , fsUsedInodes  :: !Integer
    , fsFreeInodes  :: !Integer
    , fsAvailInodes :: !Integer
    , fsIOSize      :: !Integer
    , fsBlockSize   :: !Integer
    , fsTotalBlocks :: !Integer
    , fsFreeBlocks  :: !Integer
    , fsUsedBlocks  :: !Integer
    , fsAvailBlocks :: !Integer
    , fsSystime     :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

data DiskIO = DiskIO
    { diskName    :: !ByteString
    , diskRead    :: !Integer
    , diskWrite   :: !Integer
    , diskSystime :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

data NetworkIO = NetworkIO
    { ifaceIOName     :: !ByteString
    , ifaceTX         :: !Integer
    , ifaceRX         :: !Integer
    , ifaceIPackets   :: !Integer
    , ifaceOPackets   :: !Integer
    , ifaceIErrors    :: !Integer
    , ifaceOErrors    :: !Integer
    , ifaceCollisions :: !Integer
    , ifaceSystem     :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

newtype InterfaceMode = InterfaceMode CInt
    deriving
        ( Eq
        , Ord
        , Enum
        , Bounded
        , Integral
        , Num
        , Real
        , Show
        , Storable
        , Generic
        )

newtype InterfaceStatus = InterfaceStatus CInt
    deriving
        ( Eq
        , Ord
        , Enum
        , Bounded
        , Integral
        , Num
        , Real
        , Show
        , Storable
        , Generic
        )

data NetworkInterface = NetworkInterface
    { ifaceName    :: !ByteString
    , ifaceSpeed   :: !Integer
    , ifaceFactor  :: !Integer
    , ifaceDuplex  :: !InterfaceMode
    , ifaceUp      :: !InterfaceStatus
    , ifaceSystime :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

data Page = Page
    { pagesIn      :: !Integer
    , pagesOut     :: !Integer
    , pagesSysTime :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

newtype ProcessState = ProcessState CInt
    deriving
        ( Eq
        , Ord
        , Enum
        , Bounded
        , Integral
        , Num
        , Real
        , Show
        , Storable
        , Generic
        )

data Process = Process
    { procName        :: !ByteString
    , procTitle       :: !ByteString
    , procPid         :: !Integer
    , procParent      :: !Integer
    , procPGid        :: !Integer
    , procSessId      :: !Integer
    , procUid         :: !Integer
    , procEUid        :: !Integer
    , procGid         :: !Integer
    , procEGid        :: !Integer
    , procSwitches    :: !Integer
    , procVoluntary   :: !Integer
    , procInvoluntary :: !Integer
    , procSize        :: !Integer
    , procResident    :: !Integer
    , procStart       :: !POSIXTime
    , procSpent       :: !POSIXTime
    , procCPUPercent  :: !Double
    , procNice        :: !Integer
    , procState       :: !ProcessState
    , procSystime     :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

newtype ProcessSource = ProcessSource CInt
    deriving
        ( Eq
        , Ord
        , Enum
        , Bounded
        , Integral
        , Num
        , Real
        , Show
        , Storable
        , Generic
        )

data ProcessCount = ProcessCount
    { countTotal    :: !Integer
    , countRunning  :: !Integer
    , countSleeping :: !Integer
    , countStopped  :: !Integer
    , countZombie   :: !Integer
    , countUnknown  :: !Integer
    , countSystime  :: !POSIXTime
    } deriving (Eq, Ord, Show, Generic)

infixl 4 <%>, <#>, <@>, <!>

(<%>) :: (Integral a, Applicative f) => (Integer -> b) -> a -> f b
(<%>) a b = a <$> pure (toInteger b)

(<#>) :: (Integral a, Applicative f) => f (Integer -> b) -> a -> f b
(<#>) a b = a <*> pure (toInteger b)

(<@>) :: (Fractional a, Real c, Applicative f) => f (a -> b) -> c -> f b
(<@>) a b = a <*> pure (realToFrac b)

(<!>) :: Applicative f => f (a -> b) -> a -> f b
(<!>) a b = a <*> pure b
