{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : System.Statgrab.Base
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.Statgrab.Base where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <statgrab.h>

--
-- Enums
--

newtype Error = Error { errorNumber :: CInt }
    deriving (Eq, Show)

#{enum Error, Error
  , errNone               = SG_ERROR_NONE
  , errAsPrintf           = SG_ERROR_ASPRINTF
  , errDevices            = SG_ERROR_DEVICES
  , errDevStatGetDevs     = SG_ERROR_DEVSTAT_GETDEVS
  , errDevstat_selectdevs = SG_ERROR_DEVSTAT_SELECTDEVS
  , errDiskinfo           = SG_ERROR_DISKINFO
  , errEnoent             = SG_ERROR_ENOENT
  , errGetifaddrs         = SG_ERROR_GETIFADDRS
  , errGetmountInfo       = SG_ERROR_GETMNTINFO
  , errGetPageSize        = SG_ERROR_GETPAGESIZE
  , errHost               = SG_ERROR_HOST
  , errKstatDataLookup    = SG_ERROR_KSTAT_DATA_LOOKUP
  , errKstatLookup        = SG_ERROR_KSTAT_LOOKUP
  , errKstatOpen          = SG_ERROR_KSTAT_OPEN
  , errKstatRead          = SG_ERROR_KSTAT_READ
  , errKvmGetSwapInfo     = SG_ERROR_KVM_GETSWAPINFO
  , errKvmOpenFiles       = SG_ERROR_KVM_OPENFILES
  , errMalloc             = SG_ERROR_MALLOC
  , errMemStatus          = SG_ERROR_MEMSTATUS
  , errOpen               = SG_ERROR_OPEN
  , errOpenDir            = SG_ERROR_OPENDIR
  , errParse              = SG_ERROR_PARSE
  , errPdhAdd             = SG_ERROR_PDHADD
  , errPdhCollect         = SG_ERROR_PDHCOLLECT
  , errPdhOpen            = SG_ERROR_PDHOPEN
  , errPdhRead            = SG_ERROR_PDHREAD
  , errPermission         = SG_ERROR_PERMISSION
  , errPStat              = SG_ERROR_PSTAT
  , errSetEGid            = SG_ERROR_SETEGID
  , errSetEUid            = SG_ERROR_SETEUID
  , errSetMntent          = SG_ERROR_SETMNTENT
  , errSocket             = SG_ERROR_SOCKET
  , errSwapCtl            = SG_ERROR_SWAPCTL
  , errSysConf            = SG_ERROR_SYSCONF
  , errSysCtl             = SG_ERROR_SYSCTL
  , errSysCtlByName       = SG_ERROR_SYSCTLBYNAME
  , errSysctlNameToMib    = SG_ERROR_SYSCTLNAMETOMIB
  , errUname              = SG_ERROR_UNAME
  , errUnsupported        = SG_ERROR_UNSUPPORTED
  , errXswVerMismatch     = SG_ERROR_XSW_VER_MISMATCH
}

newtype Duplex = Duplex { duplex :: CInt }
    deriving (Eq, Show)

#{enum Duplex, Duplex
  , duplexFull    = SG_IFACE_DUPLEX_FULL
  , duplexHalf    = SG_IFACE_DUPLEX_HALF
  , duplexUnknown = SG_IFACE_DUPLEX_UNKNOWN
}

newtype ProcessState = ProcessState { processState :: CInt }
    deriving (Eq, Show)

#{enum ProcessState, ProcessState
  , stateRunning  = SG_PROCESS_STATE_RUNNING
  , stateSleeping = SG_PROCESS_STATE_SLEEPING
  , stateStopped  = SG_PROCESS_STATE_STOPPED
  , stateZombie   = SG_PROCESS_STATE_ZOMBIE
  , stateUnknown  = SG_PROCESS_STATE_UNKNOWN
}

--
-- Structs
--

data HostInfo = HostInfo
    { infoOsName    :: {-# UNPACK #-} !CString
    , infoOsRelease :: {-# UNPACK #-} !CString
    , infoOsVersion :: {-# UNPACK #-} !CString
    , infoPlatform  :: {-# UNPACK #-} !CString
    , infoHostname  :: {-# UNPACK #-} !CString
    , infoUptime    :: {-# UNPACK #-} !CTime
    }

instance Storable HostInfo where

data CpuStats = CpuStats
    { cpuUser    :: {-# UNPACK #-} !CLLong
    , cpuKernel  :: {-# UNPACK #-} !CLLong
    , cpuIdle    :: {-# UNPACK #-} !CLLong
    , cpuIoWait  :: {-# UNPACK #-} !CLLong
    , cpuSwap    :: {-# UNPACK #-} !CLLong
    , cpuNice    :: {-# UNPACK #-} !CLLong
    , cpuTotal   :: {-# UNPACK #-} !CLLong
    , cpuSysTime :: {-# UNPACK #-} !CTime
    }

instance Storable CpuStats where

data CpuPercents = CpuPercents
    { cpuPctUser      :: {-# UNPACK #-} !CFloat
    , cpuPctKernel    :: {-# UNPACK #-} !CFloat
    , cpuPctIdle      :: {-# UNPACK #-} !CFloat
    , cpuPctIoWait    :: {-# UNPACK #-} !CFloat
    , cpuPctSwap      :: {-# UNPACK #-} !CFloat
    , cpuPctNice      :: {-# UNPACK #-} !CFloat
    , cpuPctTimeTaken :: {-# UNPACK #-} !CTime
    }

instance Storable CpuPercents where

data MemStats = MemStats
    { memTotal :: {-# UNPACK #-} !CLLong
    , memFree  :: {-# UNPACK #-} !CLLong
    , memUsed  :: {-# UNPACK #-} !CLLong
    , memCache :: {-# UNPACK #-} !CLLong
    }

instance Storable MemStats where

data LoadStats = LoadStats
    { load1  :: {-# UNPACK #-} !CDouble
    , load5  :: {-# UNPACK #-} !CDouble
    , load15 :: {-# UNPACK #-} !CDouble
    }

instance Storable LoadStats where

data UserStats = UserStats
    { userNameList   :: {-# UNPACK #-} !CString
    , userNumEntries :: {-# UNPACK #-} !CInt
    }

instance Storable UserStats where

data SwapStats = SwapStats
    { swapTotal :: {-# UNPACK #-} !CLLong
    , swapUsed  :: {-# UNPACK #-} !CLLong
    , swapFRee  :: {-# UNPACK #-} !CLLong
    }

instance Storable SwapStats where

data FsStats = FsStats
    { fsDeviceName  :: {-# UNPACK #-} !CString
    , fsType        :: {-# UNPACK #-} !CString
    , fsMountPoint  :: {-# UNPACK #-} !CString
    , fsSize        :: {-# UNPACK #-} !CLLong
    , fsUsed        :: {-# UNPACK #-} !CLLong
    , fsAvail       :: {-# UNPACK #-} !CLLong
    , fsTotalInodes :: {-# UNPACK #-} !CLLong
    , fsUsedInodes  :: {-# UNPACK #-} !CLLong
    , fsFreeInodes  :: {-# UNPACK #-} !CLLong
    , fsAvailInodes :: {-# UNPACK #-} !CLLong
    , fsIOSize      :: {-# UNPACK #-} !CLLong
    , fsBlockSize   :: {-# UNPACK #-} !CLLong
    , fsTotalBlocks :: {-# UNPACK #-} !CLLong
    , fsFreeBlocks  :: {-# UNPACK #-} !CLLong
    , fsUsedBlocks  :: {-# UNPACK #-} !CLLong
    , fsAvailBlocks :: {-# UNPACK #-} !CLLong
    }

instance Storable FsStats where

data DiskIOStats = DiskIOStats
    { diskName       :: {-# UNPACK #-} !CString
    , diskReadBytes  :: {-# UNPACK #-} !CLLong
    , diskWriteBytes :: {-# UNPACK #-} !CLLong
    , diskSysTime    :: {-# UNPACK #-} !CTime
    }

instance Storable DiskIOStats where

data NetworkIOStats = NetworkIOStats
    { ifaceName       :: {-# UNPACK #-} !CString
    , ifaceTX         :: {-# UNPACK #-} !CString
    , ifaceRX         :: {-# UNPACK #-} !CString
    , ifaceIPackets   :: {-# UNPACK #-} !CString
    , ifaceOPackets   :: {-# UNPACK #-} !CString
    , ifaceIErrors    :: {-# UNPACK #-} !CString
    , ifaceOErrors    :: {-# UNPACK #-} !CString
    , ifaceCollisions :: {-# UNPACK #-} !CString
    , ifaceSystem     :: {-# UNPACK #-} !CTime
    }

instance Storable NetworkIOStats where

data NetworkIFaceStats = NetworkIFaceStats
    { ifaceStatsName :: {-# UNPACK #-} !CString
    , ifaceSpeed     :: {-# UNPACK #-} !CInt
    , ifaceDuplex    :: {-# UNPACK #-} !Duplex
    , ifaceUp        :: {-# UNPACK #-} !CInt
    }

instance Storable NetworkIFaceStats where

data PageStats = PageStats
    { pagesIn  :: {-# UNPACK #-} !CLLong
    , pagesOut :: {-# UNPACK #-} !CLLong
    , systime  :: {-# UNPACK #-} !CTime
    }

instance Storable PageStats where

data ProcessStats = ProcessStats
    { procName       :: {-# UNPACK #-} !CString
    , procTitle      :: {-# UNPACK #-} !Cstring
    , procPid        :: {-# UNPACK #-} !CInt
    , procParent     :: {-# UNPACK #-} !Cint
    , procPGid       :: {-# UNPACK #-} !CInt
    , procUid        :: {-# UNPACK #-} !CUInt
    , procEUid       :: {-# UNPACK #-} !CUInt
    , procGid        :: {-# UNPACK #-} !CUInt
    , procEGid       :: {-# UNPACK #-} !CUInt
    , procSize       :: {-# UNPACK #-} !CULong
    , procResident   :: {-# UNPACK #-} !CULong
    , procTimeSpent  :: {-# UNPACK #-} !CLong
    , procCpuPercent :: {-# UNPACK #-} !CDouble
    , procNice       :: {-# UNPACK #-} !CInt
    , procSTate      :: {-# UNPACK #-} !ProcessState
    }

instance Storable ProcessStats where

data ProcessCount = ProcessCount
    { procTotal    :: {-# UNPACK #-} !CInt
    , procRunning  :: {-# UNPACK #-} !CInt
    , procSleeping :: {-# UNPACK #-} !CInt
    , procStopped  :: {-# UNPACK #-} !CInt
    , procZombie   :: {-# UNPACK #-} !CInt
    }

instance Storable ProcessStats where

--
-- Foreign Calls
--

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_get_host_info :: IO HostInfo

foreign import ccall safe "statgrab.h sg_get_cpu_stats"
    sg_get_cpu_stats :: IO CpuStats

foreign import ccall safe "statgrab.h sg_get_cpu_stats_diff"
    sg_get_cpu_stats_diff :: IO CpuStats

foreign import ccall safe "statgrab.h sg_get_cpu_percents"
    sg_get_cpu_stats_percents :: IO CpuPercents

foreign import ccall safe "statgrab.h sg_get_mem_stats"
    sg_get_mem_stats :: IO MemStats

foreign import ccall safe "statgrab.h sg_get_load_stats"
    sg_get_load_stats :: IO LoadStats

foreign import ccall safe "statgrab.h sg_get_user_stats"
    sg_get_user_stats :: IO UserStats

foreign import ccall safe "statgrab.h sg_get_disk_io_stats"
    sg_get_swap_stats :: IO SwapStats

foreign import ccall safe "statgrab.h sg_get_fs_stats"
    sg_get_fs_stats :: CInt -> IO FsStats

foreign import ccall safe "statgrab.h sg_get_disk_io_stats"
    sg_get_disk_io_stats :: CInt -> IO DiskIOStats

foreign import ccall safe "statgrab.h sg_get_disk_io_stats_diff"
    sg_get_disk_io_stats_diff :: CInt -> IO DiskIOStats

foreign import ccall safe "statgrab.h sg_get_network_io_stats"
    sg_get_network_io_stats :: CInt -> IO NetworkIOStats

foreign import ccall safe "statgrab.h sg_get_network_io_stats_diff"
    sg_get_network_io_stats_diff :: CInt -> IO NetworkIOStats

foreign import ccall safe "statgrab.h sg_get_network_iface_stats"
    sg_get_network_iface_stats :: CInt -> IO NetworkIFaceStats

foreign import ccall safe "statgrab.h sg_get_page_stats"
    sg_get_page_stats :: IO PageStats

foreign import ccall safe "statgrab.h sg_get_page_stats_diff"
    sg_get_page_stats_diff :: IO PageStats

foreign import ccall safe "statgrab.h sg_get_process_stats"
    sg_get_process_stats :: CInt -> IO ProcessStats

foreign import ccall safe "statgrab.h sg_get_process_count"
    sg_get_process_count :: IO ProcessCount
