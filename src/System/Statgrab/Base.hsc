{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : System.Statgrab.Base
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module System.Statgrab.Base where

import Control.Applicative
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Statgrab.Types

-- newtype Duplex = Duplex { duplex :: CInt }
--     deriving (Eq, Ord, Storable)

-- #{enum Duplex, Duplex
--   , duplexFull    = SG_IFACE_DUPLEX_FULL
--   , duplexHalf    = SG_IFACE_DUPLEX_HALF
--   , duplexUnknown = SG_IFACE_DUPLEX_UNKNOWN
-- }

-- newtype ProcessState = ProcessState { processState :: CInt }
--     deriving (Eq, Ord, Storable)

-- #{enum ProcessState, ProcessState
--   , stateRunning  = SG_PROCESS_STATE_RUNNING
--   , stateSleeping = SG_PROCESS_STATE_SLEEPING
--   , stateStopped  = SG_PROCESS_STATE_STOPPED
--   , stateZombie   = SG_PROCESS_STATE_ZOMBIE
--   , stateUnknown  = SG_PROCESS_STATE_UNKNOWN
-- }

#include <statgrab.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type Entries = Ptr CSize

newtype Error = Error { unError :: CInt }
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
  , errGetMsg             = SG_ERROR_GETMSG
  , errPutMsg             = SG_ERROR_PUTMSG
  , errInitialisation     = SG_ERROR_INITIALISATION
  , errMutexLock          = SG_ERROR_MUTEX_LOCK
  , errMutexUnlock        = SG_ERROR_MUTEX_UNLOCK
}

foreign import ccall safe "statgrab.h sg_get_error"
    sg_get_error :: IO Error

newtype HostState = HostState { unHostState :: CInt }
    deriving (Eq, Show, Storable)

#{enum HostState, HostState
    , stateUnknownConfig   = sg_unknown_configuration
    , statePhysicalHost    = sg_physical_host
    , stateVirtual         = sg_virtual_machine
    , stateParaVirtual     = sg_paravirtual_machine
    , stateHardwareVirtual = sg_hardware_virtualized
}

data HostInfo = HostInfo
    { hostOsName    :: {-# UNPACK #-} !CString
    , hostOsRelease :: {-# UNPACK #-} !CString
    , hostOsVersion :: {-# UNPACK #-} !CString
    , hostPlatform  :: {-# UNPACK #-} !CString
    , hostName      :: {-# UNPACK #-} !CString
    , hostBitWidth  :: {-# UNPACK #-} !CUInt
    , hostState     :: {-# UNPACK #-} !HostState
    , hostNCPU      :: {-# UNPACK #-} !CUInt
    , hostMaxCPU    :: {-# UNPACK #-} !CUInt
    , hostUptime    :: {-# UNPACK #-} !CTime
    , hostSystime   :: {-# UNPACK #-} !CTime
    } deriving (Eq, Show)

instance Storable HostInfo where
    alignment _ = #{alignment sg_host_info}
    sizeOf    _ = #{size      sg_host_info}

    peek p = HostInfo
        <$> #{peek sg_host_info, os_name} p
        <*> #{peek sg_host_info, os_release} p
        <*> #{peek sg_host_info, os_version} p
        <*> #{peek sg_host_info, platform} p
        <*> #{peek sg_host_info, hostname} p
        <*> #{peek sg_host_info, bitwidth} p
        <*> #{peek sg_host_info, host_state} p
        <*> #{peek sg_host_info, ncpus} p
        <*> #{peek sg_host_info, maxcpus} p
        <*> #{peek sg_host_info, uptime} p
        <*> #{peek sg_host_info, systime} p

    poke p HostInfo{..} = do
        #{poke sg_host_info, os_name} p hostOsName
        #{poke sg_host_info, os_release} p hostOsRelease
        #{poke sg_host_info, os_version} p hostOsVersion
        #{poke sg_host_info, platform} p hostPlatform
        #{poke sg_host_info, hostname} p hostName
        #{poke sg_host_info, bitwidth} p hostBitWidth
        #{poke sg_host_info, host_state} p hostState
        #{poke sg_host_info, ncpus} p hostNCPU
        #{poke sg_host_info, maxcpus} p hostMaxCPU
        #{poke sg_host_info, uptime} p hostUptime
        #{poke sg_host_info, systime} p hostSystime

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_get_host_info :: Ptr CSize -> IO (Ptr HostInfo)

foreign import ccall safe "statgrab.h sg_get_host_info_r"
    sg_get_host_info_ :: Ptr CSize -> IO (Ptr HostInfo)

data CpuStats = CpuStats
    { cpuUser                   :: {-# UNPACK #-} !CLLong
    , cpuKernel                 :: {-# UNPACK #-} !CLLong
    , cpuIdle                   :: {-# UNPACK #-} !CLLong
    , cpuIOWait                 :: {-# UNPACK #-} !CLLong
    , cpuSwap                   :: {-# UNPACK #-} !CLLong
    , cpuNice                   :: {-# UNPACK #-} !CLLong
    , cpuTotal                  :: {-# UNPACK #-} !CLLong
    , cpuCtxSwitches            :: {-# UNPACK #-} !CLLong
    , cpuVoluntaryCtxSwitches   :: {-# UNPACK #-} !CLLong
    , cpuInvoluntaryCtxSwitches :: {-# UNPACK #-} !CLLong
    , cpuSyscalls               :: {-# UNPACK #-} !CLLong
    , cpuInterrupts             :: {-# UNPACK #-} !CLLong
    , cpuSoftInterrupts         :: {-# UNPACK #-} !CLLong
    , cpuSystime                :: {-# UNPACK #-} !CTime
    }

instance Storable CpuStats where
    alignment _ = #{alignment sg_cpu_stats}
    sizeOf    _ = #{size      sg_cpu_stats}

    peek p = CpuStats
        <$> #{peek sg_cpu_stats, user} p
        <*> #{peek sg_cpu_stats, kernel} p
        <*> #{peek sg_cpu_stats, idle} p
        <*> #{peek sg_cpu_stats, iowait} p
        <*> #{peek sg_cpu_stats, swap} p
        <*> #{peek sg_cpu_stats, nice} p
        <*> #{peek sg_cpu_stats, total} p
        <*> #{peek sg_cpu_stats, context_switches} p
        <*> #{peek sg_cpu_stats, voluntary_context_switches} p
        <*> #{peek sg_cpu_stats, involuntary_context_switches} p
        <*> #{peek sg_cpu_stats, syscalls} p
        <*> #{peek sg_cpu_stats, interrupts} p
        <*> #{peek sg_cpu_stats, soft_interrupts} p
        <*> #{peek sg_cpu_stats, systime} p

    poke p CpuStats{..} = do
        #{poke sg_cpu_stats, user} p
        #{poke sg_cpu_stats, kernel} p
        #{poke sg_cpu_stats, idle} p
        #{poke sg_cpu_stats, iowait} p
        #{poke sg_cpu_stats, swap} p
        #{poke sg_cpu_stats, nice} p
        #{poke sg_cpu_stats, total} p
        #{poke sg_cpu_stats, context_switches} p cpuCtxSwitches
        #{poke sg_cpu_stats, voluntary_context_switches} p cpuVoluntaryCtxSwitches
        #{poke sg_cpu_stats, involuntary_context_switches} p cpuInvoluntaryCtxSwitches
        #{poke sg_cpu_stats, syscalls} p cpuSyscalls
        #{poke sg_cpu_stats, interrupts} p cpuInterrupts
        #{poke sg_cpu_stats, soft_interrupts} p cpuSoftInterrupts
        #{poke sg_cpu_stats, systime} p cpuSystime

foreign import ccall safe "statgrab.h sg_get_cpu_stats"
    sg_get_cpu_stats :: Entries -> IO (Ptr CpuStats)

foreign import ccall safe "statgrab.h sg_get_cpu_stats_diff"
    sg_get_cpu_stats_diff :: Entries -> IO (Ptr CpuStats)

foreign import ccall safe "statgrab.h sg_get_cpu_stats_r"
    sg_get_cpu_stats_r :: Entries -> IO (Ptr CpuStats)

foreign import ccall safe "statgrab.h sg_get_cpu_stats_diff_between"
    sg_get_cpu_stats_diff_between ::
        Ptr CpuStats -> Ptr CpuStats -> Entries -> IO (Ptr CpuStats)

data CpuPercents = CpuPercents
    { cpuPctUser      :: {-# UNPACK #-} !CDouble
    , cpuPctKernel    :: {-# UNPACK #-} !CDouble
    , cpuPctIdle      :: {-# UNPACK #-} !CDouble
    , cpuPctIOWait    :: {-# UNPACK #-} !CDouble
    , cpuPctSwap      :: {-# UNPACK #-} !CDouble
    , cpuPctNice      :: {-# UNPACK #-} !CDouble
    , cpuPctTimeTaken :: {-# UNPACK #-} !CTime
    }

instance Storable CpuPercents where
    alignment _ = #{alignment sg_cpu_percents}
    sizeOf    _ = #{size      sg_cpu_percents}

    peek p = CpuPercents
        <$> #{peek sg_cpu_percents, user} p
        <*> #{peek sg_cpu_percents, kernel} p
        <*> #{peek sg_cpu_percents, idle} p
        <*> #{peek sg_cpu_percents, iowait} p
        <*> #{peek sg_cpu_percents, swap} p
        <*> #{peek sg_cpu_percents, nice} p
        <*> #{peek sg_cpu_percents, time_taken} p

    poke p CpuPercents{..} = do
        #{poke sg_cpu_percents, user} p cpuPctUser
        #{poke sg_cpu_percents, kernel} p cpuPctKernel
        #{poke sg_cpu_percents, idle} p cpuPctIdle
        #{poke sg_cpu_percents, iowait} p cpuPctIOWait
        #{poke sg_cpu_percents, swap} p cpuPctSwap
        #{poke sg_cpu_percents, nice} p cpuPctNice
        #{poke sg_cpu_percents, time_taken} p cpuPctTimeTaken

newtype CpuPercentSource = CpuPercentSource { unCpuPercentSource :: CInt }
    deriving (Eq, Show, Storable)

#{enum CpuPercentSource, CpuPercentSource
    , sourceEntireCPU  = sg_entire_cpu_percent
    , sourceDiffCPU    = sg_last_diff_cpu_percent
    , sourceNewDiffCPU = sg_new_diff_cpu_percent
}

foreign import ccall safe "statgrab.h sg_get_cpu_percents_of"
    sg_get_cpu_percents_of :: CpuPercentSource -> Entries -> IO (Ptr CpuPercents)

foreign import ccall safe "statgrab.h sg_get_cpu_percents_r"
    sg_get_cpu_percents_r :: CpuStats -> Entries -> IO (Ptr CpuPercents)

data MemStats = MemStats
    { memTotal   :: {-# UNPACK #-} !CULong
    , memFree    :: {-# UNPACK #-} !CULong
    , memUsed    :: {-# UNPACK #-} !CULong
    , memCache   :: {-# UNPACK #-} !CULong
    , memSystime :: {-# UNPACK #-} !CTime
    }

instance Storable MemStats where
    alignment _ = #{alignment sg_mem_stats}
    sizeOf    _ = #{size      sg_mem_stats}

    peek p = MemStats
        <$> #{peek sg_mem_stats, total} p
        <*> #{peek sg_mem_stats, free} p
        <*> #{peek sg_mem_stats, used} p
        <*> #{peek sg_mem_stats, cache} p
        <*> #{peek sg_mem_stats, systime} p

    poke p MemStats{..} = do
        #{poke sg_mem_stats, total} p memTotal
        #{poke sg_mem_stats, free} p memFree
        #{poke sg_mem_stats, used} p memUsed
        #{poke sg_mem_stats, systime} p memSystime

foreign import ccall safe "statgrab.h sg_get_mem_stats"
    sg_get_mem_stats :: Entries -> IO (Ptr MemStats)

foreign import ccall safe "statgrab.h sg_get_mem_stats_r"
    sg_get_mem_stats_r :: Entries -> IO (Ptr MemStats)

data LoadStats = LoadStats
    { load1       :: {-# UNPACK #-} !CDouble
    , load5       :: {-# UNPACK #-} !CDouble
    , load15      :: {-# UNPACK #-} !CDouble
    , loadSystime :: {-# UNPACK #-} !CTime
    }

instance Storable LoadStats where
    alignment _ = #{alignment sg_load_stats}
    sizeOf    _ = #{size      sg_load_stats}

    peek p = LoadStats
        <$> #{peek sg_load_stats, min1} p
        <*> #{peek sg_load_stats, min5} p
        <*> #{peek sg_load_stats, min15} p
        <*> #{peek sg_load_stats, systime} p

    poke p LoadStats{..} = do
        #{poke sg_load_stats, min1} p load1
        #{poke sg_load_stats, min5} p load5
        #{poke sg_load_stats, min15} p load15
        #{poke sg_load_stats, systime} p loadSystime

foreign import ccall safe "statgrab.h sg_get_load_stats"
    sg_get_load_stats :: Entries -> IO (Ptr LoadStats)

foreign import ccall safe "statgrab.h sg_get_load_stats_r"
    sg_get_load_stats_r :: Entries -> IO (Ptr LoadStats)

data UserStats = UserStats
    { userLoginName    :: {-# UNPACK #-} !CString
    , userRecordId     :: {-# UNPACK #-} !CString
    , userRecordIdSize :: {-# UNPACK #-} !CSize
    , userDevice       :: {-# UNPACK #-} !CString
    , userHostName     :: {-# UNPACK #-} !CString
    , userPid          :: {-# UNPACK #-} !CInt
    , userLoginTime    :: {-# UNPACK #-} !CTime
    , userSystime      :: {-# UNPACK #-} !CTime
    }

instance Storable UserStats where
    alignment _ = #{alignment sg_user_stats}
    sizeOf    _ = #{size      sg_user_stats}

    peek p = UserStats
        <$> #{peek sg_user_stats, login_name} p
        <*> #{peek sg_user_stats, record_id} p
        <*> #{peek sg_user_stats, record_id_size} p
        <*> #{peek sg_user_stats, device} p
        <*> #{peek sg_user_stats, hostname} p
        <*> #{peek sg_user_stats, pid} p
        <*> #{peek sg_user_stats, login_time} p
        <*> #{peek sg_user_stats, systime} p

    poke p UserStats{..} = do
        #{poke sg_user_stats, login_name} p userLoginTime
        #{poke sg_user_stats, record_id} p userRecordId
        #{poke sg_user_stats, record_id_size} p userRecordIdSize
        #{poke sg_user_stats, device} p userDevice
        #{poke sg_user_stats, hostname} p userHostName
        #{poke sg_user_stats, pid} p userPid
        #{poke sg_user_stats, login_time} p userLoginTime
        #{poke sg_user_stats, systime} p userSystime

foreign import ccall safe "statgrab.h sg_get_user_stats"
    sg_get_user_stats :: Entries -> IO (Ptr UserStats)

foreign import ccall safe "statgrab.h sg_get_user_stats_r"
    sg_get_user_stats_r :: Entries -> IO (Ptr UserStats)

data SwapStats = SwapStats
    { swapTotal   :: {-# UNPACK #-} !CULong
    , swapUsed    :: {-# UNPACK #-} !CULong
    , swapFree    :: {-# UNPACK #-} !CULong
    , swapSystime :: {-# UNPACK #-} !CTime
    }

instance Storable SwapStats where
    alignment _ = #{alignment sg_swap_stats}
    sizeOf    _ = #{size      sg_swap_stats}

    peek p = SwapStats
        <$> #{peek sg_swap_stats, total} p
        <*> #{peek sg_swap_stats, used} p
        <*> #{peek sg_swap_stats, free} p
        <*> #{peek sg_swap_stats, systime} p

    poke p SwapStats{..} = do
        #{poke sg_swap_stats, total} p swapTotal
        #{poke sg_swap_stats, used} p swapUsed
        #{poke sg_swap_stats, free} p swapFree
        #{poke sg_swap_stats, systime} p swapSystime

foreign import ccall safe "statgrab.h sg_get_swap_stats"
    sg_get_swap_stats :: Entries -> IO (Ptr SwapStats)

foreign import ccall safe "statgrab.h sg_get_swap_stats_r"
    sg_get_swap_stats_r :: Entries -> IO (Ptr SwapStats)

newtype DeviceType = DeviceType { unDeviceType :: CInt }
    deriving (Eq, Show, Storable)

#{enum DeviceType, DeviceType
    , deviceUnknown  = sg_fs_unknown
    , deviceRegular  = sg_fs_regular
    , deviceSpecial  = sg_fs_special
    , deviceLoopback = sg_fs_loopback
    , deviceRemote   = sg_fs_remote
    , deviceLocal    = sg_fs_local
    , deviceAllTypes = sg_fs_alltypes
}

data FSStats = FSStats
    { fsDeviceName  :: {-# UNPACK #-} !CString
    , fsType        :: {-# UNPACK #-} !CString
    , fsMountPoint  :: {-# UNPACK #-} !CString
    , fsDeviceType  :: {-# UNPACK #-} !DeviceType
    , fsSize        :: {-# UNPACK #-} !CULong
    , fsUsed        :: {-# UNPACK #-} !CULong
    , fsFree        :: {-# UNPACK #-} !CULong
    , fsAvail       :: {-# UNPACK #-} !CULong
    , fsTotalInodes :: {-# UNPACK #-} !CULong
    , fsUsedInodes  :: {-# UNPACK #-} !CULong
    , fsFreeInodes  :: {-# UNPACK #-} !CULong
    , fsAvailInodes :: {-# UNPACK #-} !CULong
    , fsIOSize      :: {-# UNPACK #-} !CULong
    , fsBlockSize   :: {-# UNPACK #-} !CULong
    , fsTotalBlocks :: {-# UNPACK #-} !CULong
    , fsFreeBlocks  :: {-# UNPACK #-} !CULong
    , fsUsedBlocks  :: {-# UNPACK #-} !CULong
    , fsAvailBlocks :: {-# UNPACK #-} !CULong
    , fsSystime     :: {-# UNPACK #-} !CTime
    }

instance Storable FSStats where
    alignment _ = #{alignment sg_fs_stats}
    sizeOf    _ = #{size      sg_fs_stats}

    { fsDeviceName  :: {-# UNPACK #-} !CString
    , fsType        :: {-# UNPACK #-} !CString
    , fsMountPoint  :: {-# UNPACK #-} !CString
    , fsDeviceType  :: {-# UNPACK #-} !DeviceType
    , fsSize        :: {-# UNPACK #-} !CULong
    , fsUsed        :: {-# UNPACK #-} !CULong
    , fsFree        :: {-# UNPACK #-} !CULong
    , fsAvail       :: {-# UNPACK #-} !CULong
    , fsTotalInodes :: {-# UNPACK #-} !CULong
    , fsUsedInodes  :: {-# UNPACK #-} !CULong
    , fsFreeInodes  :: {-# UNPACK #-} !CULong
    , fsAvailInodes :: {-# UNPACK #-} !CULong
    , fsIOSize      :: {-# UNPACK #-} !CULong
    , fsBlockSize   :: {-# UNPACK #-} !CULong
    , fsTotalBlocks :: {-# UNPACK #-} !CULong
    , fsFreeBlocks  :: {-# UNPACK #-} !CULong
    , fsUsedBlocks  :: {-# UNPACK #-} !CULong
    , fsAvailBlocks :: {-# UNPACK #-} !CULong
    , fsSystime     :: {-# UNPACK #-} !CTime
    }

    peek p = FSStats
        <$> #{peek sg_fs_stats, device_name} p
        <*> #{peek sg_fs_stats, fs_type} p
        <*> #{peek sg_fs_stats, mnt_point} p
        <*> #{peek sg_fs_stats, device_type} p
        <*> #{peek sg_fs_stats, size} p
        <*> #{peek sg_fs_stats, used} p
        <*> #{peek sg_fs_stats, free} p
        <*> #{peek sg_fs_stats, avail} p
        <*> #{peek sg_fs_stats, total_inodes} p
        <*> #{peek sg_fs_stats, used_inodes} p
        <*> #{peek sg_fs_stats, free_inodes} p
        <*> #{peek sg_fs_stats, avail_inodes} p
        <*> #{peek sg_fs_stats, io_size} p
        <*> #{peek sg_fs_stats, block_size} p
        <*> #{peek sg_fs_stats, total_blocks} p
        <*> #{peek sg_fs_stats, free_blocks} p
        <*> #{peek sg_fs_stats, used_blocks} p
        <*> #{peek sg_fs_stats, avail_blocks} p
        <*> #{peek sg_fs_stats, systime} p

    poke p FSStats{..} = do
        #{poke sg_fs_stats, device_name} p fsDeviceName
        #{poke sg_fs_stats, fs_type} p fsType
        #{poke sg_fs_stats, mnt_point} p fsMountPoint
        #{poke sg_fs_stats, device_type} p fsDeviceType
        #{poke sg_fs_stats, size} p fsSize
        #{poke sg_fs_stats, user} p fsUser
        #{poke sg_fs_stats, free} p fsFree
        #{poke sg_fs_stats, avail} p fsAvail
        #{poke sg_fs_stats, total_inodes} p fsTotalInodes
        #{poke sg_fs_stats, used_inodes} p fsUsedInodes
        #{poke sg_fs_stats, free_inodes} p fsFreeInodes
        #{poke sg_fs_stats, avail_inodes} p fsAvailInodes
        #{poke sg_fs_stats, io_size} p fsIOSize
        #{poke sg_fs_stats, block_size} p fsBlockSize
        #{poke sg_fs_stats, total_blocks} p fsTotalBlocks
        #{poke sg_fs_stats, free_blocks} p fsFreeBlocks
        #{poke sg_fs_stats, used_blocks} p fsUsedBlocks
        #{poke sg_fs_stats, avail_blocks} p fsAvailBlocks
        #{poke sg_fs_stats, systime} p fsSystime

foreign import ccall safe "statgrab.h sg_get_valid_filesystems"
     sg_get_valid_filesystems :: Entries -> IO (Ptr CString)

-- foreign import ccall safe "statgrab.h sg_get_fs_val"
--      sg_set_valid_filesystems :: Ptr CString -> IO Error
-- __sg_public const char **sg_get_valid_filesystems(size_t *entries);
-- __sg_public sg_error sg_set_valid_filesystems(const char *valid_fs[]);

foreign import ccall safe "statgrab.h sg_get_fs_stats"
     sg_get_fs_stats :: Entries -> IO (Ptr FSStats)

foreign import ccall safe "statgrab.h sg_get_fs_stats_r"
     sg_get_fs_stats_r :: Entries -> IO (Ptr FSStats)

foreign import ccall safe "statgrab.h sg_get_fs_stats_diff"
     sg_get_fs_stats_diff :: Entries -> IO (Ptr FSStats)

foreign import ccall safe "statgrab.h sg_get_fs_stats_diff_between"
     sg_get_fs_stats_diff_between :: Ptr FSStats -> Ptr FSStats -> Entries -> IO (Ptr FSStats)

foreign import ccall safe "statgrab.h sg_fs_compare_device_name"
     sg_fs_compare_device_name :: Ptr () -> Ptr () -> IO (CInt)

foreign import ccall safe "statgrab.h sg_fs_compare_mnt_point"
     sg_fs_compare_mnt_point :: Ptr () -> Ptr () -> IO (CInt)

data DiskIOStats = DiskIOStats
    { diskName    :: {-# UNPACK #-} !CString
    , diskRead    :: {-# UNPACK #-} !CULong
    , diskWrite   :: {-# UNPACK #-} !CULong
    , diskSystime :: {-# UNPACK #-} !CTime
    }

instance Storable DiskIOStats where
    alignment _ = #{alignment sg_disk_io_stats}
    sizeOf    _ = #{size      sg_disk_io_stats}

    peek p = DiskIOStats
        <$> #{peek sg_disk_io_stats, disk_name} p
        <*> #{peek sg_disk_io_stats, read_bytes} p
        <*> #{peek sg_disk_io_stats, write_bytes} p
        <*> #{peek sg_disk_io_stats, systime} p

    poke p DiskIOStats{..} = do
        #{poke sg_disk_io_stats, disk_name} p diskName
        #{poke sg_disk_io_stats, read_bytes} p diskRead
        #{poke sg_disk_io_stats, write_bytes} p diskWrite
        #{poke sg_disk_io_stats, systime} p diskSystime

foreign import ccall safe "statgrab.h sg_get_disk_io_stats"
    sg_get_disk_io_stats :: Entries -> IO (Ptr DiskIOStats)

foreign import ccall safe "statgrab.h sg_get_disk_io_stats_r"
    sg_get_disk_io_stats_r :: Entries -> IO (Ptr DiskIOStats)

foreign import ccall safe "statgrab.h sg_get_disk_io_stats_diff"
    sg_get_disk_io_stats_diff :: Entries -> IO (Ptr DiskIOStats)

foreign import ccall safe "statgrab.h sg_get_disk_io_stats_diff_between"
    sg_get_disk_io_stats_diff_between ::
        Ptr DiskIOStats -> Ptr DiskIOStats -> Entries -> IO (Ptr DiskIOStats)

foreign import ccall safe "statgrab.h sg_disk_io_compare_name"
    sg_disk_io_compare_name :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_disk_io_compare_traffic"
    sg_disk_io_compare_traffic :: Ptr () -> Ptr () -> IO CInt

-- data NetworkIOStats = NetworkIOStats
--     { ifaceName       :: {-# UNPACK #-} !CString
--     , ifaceTX         :: {-# UNPACK #-} !CString
--     , ifaceRX         :: {-# UNPACK #-} !CString
--     , ifaceIPackets   :: {-# UNPACK #-} !CString
--     , ifaceOPackets   :: {-# UNPACK #-} !CString
--     , ifaceIErrors    :: {-# UNPACK #-} !CString
--     , ifaceOErrors    :: {-# UNPACK #-} !CString
--     , ifaceCollisions :: {-# UNPACK #-} !CString
--     , ifaceSystem     :: {-# UNPACK #-} !CTime
--     }

-- instance Storable NetworkIOStats where
--     alignment _ = #{alignment sg_network_io_stats}
--     sizeOf    _ = #{size      sg_network_io_stats}

--     peek p = NetworkIOStats
--         <$> #{peek sg_network_io_stats, interface_name} p
--         <*> #{peek sg_network_io_stats, tx} p
--         <*> #{peek sg_network_io_stats, rx} p
--         <*> #{peek sg_network_io_stats, ipackets} p
--         <*> #{peek sg_network_io_stats, opackets} p
--         <*> #{peek sg_network_io_stats, ierrors} p
--         <*> #{peek sg_network_io_stats, oerrors} p
--         <*> #{peek sg_network_io_stats, collisions} p
--         <*> #{peek sg_network_io_stats, systime} p

--     poke p NetworkIOStats{..} = do
--         #{poke sg_network_io_stats, interface_name} p ifaceName
--         #{poke sg_network_io_stats, tx} p ifaceTX
--         #{poke sg_network_io_stats, rx} p ifaceRX
--         #{poke sg_network_io_stats, ipackets} p ifaceIPackets
--         #{poke sg_network_io_stats, opackets} p ifaceOPackets
--         #{poke sg_network_io_stats, ierrors} p ifaceIErrors
--         #{poke sg_network_io_stats, oerrors} p ifaceOErrors
--         #{poke sg_network_io_stats, collisions} p ifaceCollisions
--         #{poke sg_network_io_stats, systime} p ifaceSystem

-- data NetworkIFaceStats = NetworkIFaceStats
--     { ifaceStatsName :: {-# UNPACK #-} !CString
--     , ifaceSpeed     :: {-# UNPACK #-} !CInt
--     , ifaceDuplex    :: {-# UNPACK #-} !Duplex
--     , ifaceUp        :: {-# UNPACK #-} !CInt
--     }

-- instance Storable NetworkIFaceStats where
--     alignment _ = #{alignment sg_network_iface_stats}
--     sizeOf    _ = #{size      sg_network_iface_stats}

--     peek p = NetworkIFaceStats
--         <$> #{peek sg_network_iface_stats, interface_name} p
--         <*> #{peek sg_network_iface_stats, speed} p
--         <*> #{peek sg_network_iface_stats, duplex} p
--         <*> #{peek sg_network_iface_stats, up} p

--     poke p NetworkIFaceStats{..} = do
--         #{poke sg_network_iface_stats, interface_name} p ifaceStatsName
--         #{poke sg_network_iface_stats, speed} p ifaceSpeed
--         #{poke sg_network_iface_stats, duplex} p ifaceDuplex
--         #{poke sg_network_iface_stats, up} p ifaceUp

-- data PageStats = PageStats
--     { pagesIn      :: {-# UNPACK #-} !CLLong
--     , pagesOut     :: {-# UNPACK #-} !CLLong
--     , pagesSysTime :: {-# UNPACK #-} !CTime
--     }

-- instance Storable PageStats where
--     alignment _ = #{alignment sg_page_stats}
--     sizeOf    _ = #{size      sg_page_stats}

--     peek p = PageStats
--         <$> #{peek sg_page_stats, pages_pagein} p
--         <*> #{peek sg_page_stats, pages_pageout} p
--         <*> #{peek sg_page_stats, systime} p

--     poke p PageStats{..} = do
--         #{poke sg_page_stats, pages_pagein} p pagesIn
--         #{poke sg_page_stats, pages_pageout} p pagesOut
--         #{poke sg_page_stats, systime} p pagesSysTime

-- data ProcessStats = ProcessStats
--     { procName       :: {-# UNPACK #-} !CString
--     , procTitle      :: {-# UNPACK #-} !CString
--     , procPid        :: {-# UNPACK #-} !CInt
--     , procParent     :: {-# UNPACK #-} !CInt
--     , procPGid       :: {-# UNPACK #-} !CInt
--     , procUid        :: {-# UNPACK #-} !CUInt
--     , procEUid       :: {-# UNPACK #-} !CUInt
--     , procGid        :: {-# UNPACK #-} !CUInt
--     , procEGid       :: {-# UNPACK #-} !CUInt
--     , procSize       :: {-# UNPACK #-} !CULong
--     , procResident   :: {-# UNPACK #-} !CULong
--     , procTimeSpent  :: {-# UNPACK #-} !CLong
--     , procCpuPercent :: {-# UNPACK #-} !CDouble
--     , procNice       :: {-# UNPACK #-} !CInt
--     , procState      :: {-# UNPACK #-} !ProcessState
--     }

-- instance Storable ProcessStats where
--     alignment _ = #{alignment sg_process_stats}
--     sizeOf    _ = #{size      sg_process_stats}

--     peek p = ProcessStats
--         <$> #{peek sg_process_stats, process_name} p
--         <*> #{peek sg_process_stats, proctitle} p
--         <*> #{peek sg_process_stats, pid} p
--         <*> #{peek sg_process_stats, parent} p
--         <*> #{peek sg_process_stats, pgid} p
--         <*> #{peek sg_process_stats, uid} p
--         <*> #{peek sg_process_stats, euid} p
--         <*> #{peek sg_process_stats, gid} p
--         <*> #{peek sg_process_stats, egid} p
--         <*> #{peek sg_process_stats, proc_size} p
--         <*> #{peek sg_process_stats, proc_resident} p
--         <*> #{peek sg_process_stats, time_spent} p
--         <*> #{peek sg_process_stats, cpu_percent} p
--         <*> #{peek sg_process_stats, nice} p
--         <*> #{peek sg_process_stats, state} p

--     poke p ProcessStats{..} = do
--         #{poke sg_process_stats, process_name} p procName
--         #{poke sg_process_stats, proctitle} p procTitle
--         #{poke sg_process_stats, pid} p procPid
--         #{poke sg_process_stats, parent} p procParent
--         #{poke sg_process_stats, pgid} p procPGid
--         #{poke sg_process_stats, uid} p procUid
--         #{poke sg_process_stats, euid} p procEUid
--         #{poke sg_process_stats, gid} p procGid
--         #{poke sg_process_stats, egid} p procEGid
--         #{poke sg_process_stats, proc_size} p procSize
--         #{poke sg_process_stats, proc_resident} p procResident
--         #{poke sg_process_stats, time_spent} p procTimeSpent
--         #{poke sg_process_stats, cpu_percent} p procCpuPercent
--         #{poke sg_process_stats, nice} p procNice
--         #{poke sg_process_stats, state} p procState

-- data ProcessCount = ProcessCount
--     { procTotal    :: {-# UNPACK #-} !CInt
--     , procRunning  :: {-# UNPACK #-} !CInt
--     , procSleeping :: {-# UNPACK #-} !CInt
--     , procStopped  :: {-# UNPACK #-} !CInt
--     , procZombie   :: {-# UNPACK #-} !CInt
--     }

-- instance Storable ProcessCount where
--     alignment _ = #{alignment sg_process_count}
--     sizeOf    _ = #{size      sg_process_count}

--     peek p = ProcessCount
--         <$> #{peek sg_process_count, total} p
--         <*> #{peek sg_process_count, running} p
--         <*> #{peek sg_process_count, sleeping} p
--         <*> #{peek sg_process_count, stopped} p
--         <*> #{peek sg_process_count, zombie} p

--     poke p ProcessCount{..} = do
--         #{poke sg_process_count, total} p procTotal
--         #{poke sg_process_count, running} p procRunning
--         #{poke sg_process_count, sleeping} p procSleeping
--         #{poke sg_process_count, stopped} p procStopped
--         #{poke sg_process_count, zombie} p procZombie

--
-- Foreign Calls
--

foreign import ccall safe "statgrab.h sg_init"
    sg_init :: CInt -> IO Error

-- foreign import ccall safe "statgrab.h sg_snapshot"
--     sg_snapshot :: IO Error

-- foreign import ccall safe "statgrab.h sg_shutdown"
--     sg_shutdown :: IO Error

-- foreign import ccall safe "statgrab.h sg_drop_privileges"
--     sg_drop_privileges :: IO CInt

-- foreign import ccall safe "statgrab.h sg_get_error"
--     sg_get_error :: IO Error

-- foreign import ccall safe "statgrab.h sg_get_error_arg"
--     sg_get_error_arg :: IO CChar

-- foreign import ccall safe "statgrab.h sg_get_error_errno"
--     sg_get_error_errno :: IO CInt

-- foreign import ccall safe "statgrab.h sg_str_error"
--     sg_str_error :: Error -> IO CChar

-- foreign import ccall safe "statgrab.h sg_get_mem_stats"
--     sg_get_mem_stats :: IO (Ptr MemStats)

-- foreign import ccall safe "statgrab.h sg_get_load_stats"
--     sg_get_load_stats :: IO (Ptr LoadStats)

-- foreign import ccall safe "statgrab.h sg_get_user_stats"
--     sg_get_user_stats :: IO (Ptr UserStats)

-- foreign import ccall safe "statgrab.h sg_get_disk_io_stats"
--     sg_get_swap_stats :: IO (Ptr SwapStats)

-- foreign import ccall safe "statgrab.h sg_get_fs_stats"
--     sg_get_fs_stats :: CInt -> IO (Ptr FSStats)

-- foreign import ccall safe "statgrab.h sg_get_disk_io_stats"
--     sg_get_disk_io_stats :: CInt -> IO (Ptr DiskIOStats)

-- foreign import ccall safe "statgrab.h sg_get_disk_io_stats_diff"
--     sg_get_disk_io_stats_diff :: CInt -> IO (Ptr DiskIOStats)

-- foreign import ccall safe "statgrab.h sg_get_network_io_stats"
--     sg_get_network_io_stats :: CInt -> IO (Ptr NetworkIOStats)

-- foreign import ccall safe "statgrab.h sg_get_network_io_stats_diff"
--     sg_get_network_io_stats_diff :: CInt -> IO (Ptr NetworkIOStats)

-- foreign import ccall safe "statgrab.h sg_get_network_iface_stats"
--     sg_get_network_iface_stats :: CInt -> IO (Ptr NetworkIFaceStats)

-- foreign import ccall safe "statgrab.h sg_get_page_stats"
--     sg_get_page_stats :: IO (Ptr PageStats)

-- foreign import ccall safe "statgrab.h sg_get_page_stats_diff"
--     sg_get_page_stats_diff :: IO (Ptr PageStats)

-- foreign import ccall safe "statgrab.h sg_get_process_stats"
--     sg_get_process_stats :: CInt -> IO (Ptr ProcessStats)

-- -- foreign import ccall safe "statgrab.h sg_get_process_count_of"
-- --     sg_get_process_count_of :: CInt -> IO (Ptr ProcessCount)
