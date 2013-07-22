{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TemplateHaskell          #-}

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

import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <statgrab.h>

--
-- Enums
--

newtype Error = Error { errorNumber :: CInt }
    deriving (Eq, Ord)

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
    deriving (Eq, Ord)

#{enum Duplex, Duplex
  , duplexFull    = SG_IFACE_DUPLEX_FULL
  , duplexHalf    = SG_IFACE_DUPLEX_HALF
  , duplexUnknown = SG_IFACE_DUPLEX_UNKNOWN
}

newtype ProcessState = ProcessState { processState :: CInt }
    deriving (Eq, Ord)

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

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data HostInfo = HostInfo
    { infoOsName    :: {-# UNPACK #-} !CString
    , infoOsRelease :: {-# UNPACK #-} !CString
    , infoOsVersion :: {-# UNPACK #-} !CString
    , infoPlatform  :: {-# UNPACK #-} !CString
    , infoHostname  :: {-# UNPACK #-} !CString
    , infoUptime    :: {-# UNPACK #-} !CTime
    }

instance Storable HostInfo where
    alignment _ = #{alignment sg_host_info}
    sizeOf    _ = #{size      sg_host_info}

    peek p = HostInfo
        <$> #{peek sg_host_info, os_name} p
        <*> #{peek sg_host_info, os_release} p
        <*> #{peek sg_host_info, os_version} p
        <*> #{peek sg_host_info, platform} p
        <*> #{peek sg_host_info, hostname} p
        <*> #{peek sg_host_info, uptime} p

    poke p HostInfo{..} = do
        #{poke sg_host_info, os_name} p infoOsName
        #{poke sg_host_info, os_release} p infoOsRelease
        #{poke sg_host_info, os_version} p infoOsVersion
        #{poke sg_host_info, platform} p infoPlatform
        #{poke sg_host_info, hostname} p infoHostname
        #{poke sg_host_info, uptime} p infoUptime

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
        <*> #{peek sg_cpu_stats, systime} p

    poke p CpuStats{..} = do
        #{poke sg_cpu_stats, user} p
        #{poke sg_cpu_stats, kernel} p
        #{poke sg_cpu_stats, idle} p
        #{poke sg_cpu_stats, iowait} p
        #{poke sg_cpu_stats, swap} p
        #{poke sg_cpu_stats, nice} p
        #{poke sg_cpu_stats, total} p
        #{poke sg_cpu_stats, systime} p

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
        #{poke sg_cpu_percents, user} p
        #{poke sg_cpu_percents, kernel} p
        #{poke sg_cpu_percents, idle} p
        #{poke sg_cpu_percents, iowait} p
        #{poke sg_cpu_percents, swap} p
        #{poke sg_cpu_percents, nice} p
        #{poke sg_cpu_percents, time_taken} p

data MemStats = MemStats
    { memTotal :: {-# UNPACK #-} !CLLong
    , memFree  :: {-# UNPACK #-} !CLLong
    , memUsed  :: {-# UNPACK #-} !CLLong
    , memCache :: {-# UNPACK #-} !CLLong
    }

instance Storable MemStats where
    alignment _ = #{alignment sg_mem_stats}
    sizeOf    _ = #{size      sg_mem_stats}

    peek p = MemStats
        <$> #{peek sg_mem_stats, total} p
        <*> #{peek sg_mem_stats, free} p
        <*> #{peek sg_mem_stats, used} p
        <*> #{peek sg_mem_stats, cache} p

    poke p MemStats{..} = do
        #{poke sg_mem_stats, total} p
        #{poke sg_mem_stats, free} p
        #{poke sg_mem_stats, used} p
        #{poke sg_mem_stats, cache} p

data LoadStats = LoadStats
    { load1  :: {-# UNPACK #-} !CDouble
    , load5  :: {-# UNPACK #-} !CDouble
    , load15 :: {-# UNPACK #-} !CDouble
    }

instance Storable LoadStats where
    alignment _ = #{alignment sg_load_stats}
    sizeOf    _ = #{size      sg_load_stats}

    peek p = LoadStats
        <$> #{peek sg_load_stats, min1} p
        <*> #{peek sg_load_stats, min5} p
        <*> #{peek sg_load_stats, min15} p

    poke p LoadStats{..} = do
        #{poke sg_load_stats, min1} p
        #{poke sg_load_stats, min5} p
        #{poke sg_load_stats, min15} p

data UserStats = UserStats
    { userNameList   :: {-# UNPACK #-} !CString
    , userNumEntries :: {-# UNPACK #-} !CInt
    }

instance Storable UserStats where
    alignment _ = #{alignment sg_user_stats}
    sizeOf    _ = #{size      sg_user_stats}

    peek p = UserStats
        <$> #{peek sg_user_stats, name_list} p
        <*> #{peek sg_user_stats, num_entries} p

    poke p UserStats{..} = do
        #{poke sg_user_stats, name_list} p
        #{poke sg_user_stats, num_entries} p

data SwapStats = SwapStats
    { swapTotal :: {-# UNPACK #-} !CLLong
    , swapUsed  :: {-# UNPACK #-} !CLLong
    , swapFRee  :: {-# UNPACK #-} !CLLong
    }

instance Storable SwapStats where
    alignment _ = #{alignment sg_swap_stats}
    sizeOf    _ = #{size      sg_swap_stats}

    peek p = SwapStats
        <$> #{peek sg_swap_stats, total} p
        <*> #{peek sg_swap_stats, used} p
        <*> #{peek sg_swap_stats, free} p

    poke p SwapStats{..} = do
        #{poke sg_swap_stats, total} p
        #{poke sg_swap_stats, used} p
        #{poke sg_swap_stats, free} p

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
    alignment _ = #{alignment sg_fs_stats}
    sizeOf    _ = #{size      sg_fs_stats}

    peek p = FsStats
        <$> #{peek sg_fs_stats, device_name} p
        <*> #{peek sg_fs_stats, fs_type} p
        <*> #{peek sg_fs_stats, mnt_point} p
        <*> #{peek sg_fs_stats, size} p
        <*> #{peek sg_fs_stats, used} p
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

    poke p FsStats{..} = do
        #{poke sg_fs_stats, device_name} p
        #{poke sg_fs_stats, fs_type} p
        #{poke sg_fs_stats, mnt_point} p
        #{poke sg_fs_stats, size} p
        #{poke sg_fs_stats, used} p
        #{poke sg_fs_stats, avail} p
        #{poke sg_fs_stats, total_inodes} p
        #{poke sg_fs_stats, used_inodes} p
        #{poke sg_fs_stats, free_inodes} p
        #{poke sg_fs_stats, avail_inodes} p
        #{poke sg_fs_stats, io_size} p
        #{poke sg_fs_stats, block_size} p
        #{poke sg_fs_stats, total_blocks} p
        #{poke sg_fs_stats, free_blocks} p
        #{poke sg_fs_stats, used_blocks} p
        #{poke sg_fs_stats, avail_blocks} p

data DiskIOStats = DiskIOStats
    { diskName       :: {-# UNPACK #-} !CString
    , diskReadBytes  :: {-# UNPACK #-} !CLLong
    , diskWriteBytes :: {-# UNPACK #-} !CLLong
    , diskSysTime    :: {-# UNPACK #-} !CTime
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
        #{poke sg_disk_io_stats, disk_name} p
        #{poke sg_disk_io_stats, read_bytes} p
        #{poke sg_disk_io_stats, write_bytes} p
        #{poke sg_disk_io_stats, systime} p

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
    alignment _ = #{alignment sg_network_io_stats}
    sizeOf    _ = #{size      sg_network_io_stats}

    peek p = NetworkIOStats
        <$> #{peek sg_network_io_stats, interface_name} p
        <*> #{peek sg_network_io_stats, tx} p
        <*> #{peek sg_network_io_stats, rx} p
        <*> #{peek sg_network_io_stats, ipackets} p
        <*> #{peek sg_network_io_stats, opackets} p
        <*> #{peek sg_network_io_stats, ierrors} p
        <*> #{peek sg_network_io_stats, oerrors} p
        <*> #{peek sg_network_io_stats, collisions} p
        <*> #{peek sg_network_io_stats, systime} p

    poke p NetworkIOStats{..} = do
        #{poke sg_network_io_stats, interface_name} p
        #{poke sg_network_io_stats, tx} p
        #{poke sg_network_io_stats, rx} p
        #{poke sg_network_io_stats, ipackets} p
        #{poke sg_network_io_stats, opackets} p
        #{poke sg_network_io_stats, ierrors} p
        #{poke sg_network_io_stats, oerrors} p
        #{poke sg_network_io_stats, collisions} p
        #{poke sg_network_io_stats, systime} p

data NetworkIFaceStats = NetworkIFaceStats
    { ifaceStatsName :: {-# UNPACK #-} !CString
    , ifaceSpeed     :: {-# UNPACK #-} !CInt
    , ifaceDuplex    :: {-# UNPACK #-} !Duplex
    , ifaceUp        :: {-# UNPACK #-} !CInt
    }

instance Storable NetworkIFaceStats where
    alignment _ = #{alignment sg_network_iface_stats}
    sizeOf    _ = #{size      sg_network_iface_stats}

    peek p = NetworkIFaceStats
        <$> #{peek sg_network_iface_stats, interface_name} p
        <*> #{peek sg_network_iface_stats, speed} p
        <*> #{peek sg_network_iface_stats, duplex} p

    poke p NetworkIFaceStats{..} = do
        #{poke sg_network_iface_stats, interface_name} p
        #{poke sg_network_iface_stats, speed} p
        #{poke sg_network_iface_stats, duplex} p

data PageStats = PageStats
    { pagesIn  :: {-# UNPACK #-} !CLLong
    , pagesOut :: {-# UNPACK #-} !CLLong
    , systime  :: {-# UNPACK #-} !CTime
    }

instance Storable PageStats where
    alignment _ = #{alignment sg_page_stats}
    sizeOf    _ = #{size      sg_page_stats}

    peek p = PageStats
        <$> #{peek sg_page_stats, pages_pagein} p
        <*> #{peek sg_page_stats, pages_pageout} p
        <*> #{peek sg_page_stats, systime} p

    poke p PageStats{..} = do
        #{poke sg_page_stats, pages_pagein} p
        #{poke sg_page_stats, pages_pageout} p
        #{poke sg_page_stats, systime} p

data ProcessStats = ProcessStats
    { procName       :: {-# UNPACK #-} !CString
    , procTitle      :: {-# UNPACK #-} !CString
    , procPid        :: {-# UNPACK #-} !CInt
    , procParent     :: {-# UNPACK #-} !CInt
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
    alignment _ = #{alignment sg_process_stats}
    sizeOf    _ = #{size      sg_process_stats}

    peek p = ProcessStats
        <$> #{peek sg_process_stats, process_name} p
        <*> #{peek sg_process_stats, proctitle} p
        <*> #{peek sg_process_stats, pid} p
        <*> #{peek sg_process_stats, parent} p
        <*> #{peek sg_process_stats, pgid} p
        <*> #{peek sg_process_stats, uid} p
        <*> #{peek sg_process_stats, euid} p
        <*> #{peek sg_process_stats, gid} p
        <*> #{peek sg_process_stats, egid} p
        <*> #{peek sg_process_stats, proc_size} p
        <*> #{peek sg_process_stats, proc_resident} p
        <*> #{peek sg_process_stats, time_spent} p
        <*> #{peek sg_process_stats, cpu_percent} p
        <*> #{peek sg_process_stats, nice} p
        <*> #{peek sg_process_stats, state} p

    poke p ProcessStats{..} = do
        #{poke sg_process_stats, process_name} p
        #{poke sg_process_stats, proctitle} p
        #{poke sg_process_stats, pid} p
        #{poke sg_process_stats, parent} p
        #{poke sg_process_stats, pgid} p
        #{poke sg_process_stats, uid} p
        #{poke sg_process_stats, euid} p
        #{poke sg_process_stats, gid} p
        #{poke sg_process_stats, egid} p
        #{poke sg_process_stats, proc_size} p
        #{poke sg_process_stats, proc_resident} p
        #{poke sg_process_stats, time_spent} p
        #{poke sg_process_stats, cpu_percent} p
        #{poke sg_process_stats, nice} p
        #{poke sg_process_stats, state} p

data ProcessCount = ProcessCount
    { procTotal    :: {-# UNPACK #-} !CInt
    , procRunning  :: {-# UNPACK #-} !CInt
    , procSleeping :: {-# UNPACK #-} !CInt
    , procStopped  :: {-# UNPACK #-} !CInt
    , procZombie   :: {-# UNPACK #-} !CInt
    }

instance Storable ProcessCount where
    alignment _ = #{alignment sg_process_count}
    sizeOf    _ = #{size      sg_process_count}

    peek p = ProcessCount
        <$> #{peek sg_process_count, total} p
        <*> #{peek sg_process_count, running} p
        <*> #{peek sg_process_count, sleeping} p
        <*> #{peek sg_process_count, stopped} p
        <*> #{peek sg_process_count, zombie} p

    poke p ProcessCount{..} = do
        #{poke sg_process_count, total} p
        #{poke sg_process_count, running} p
        #{poke sg_process_count, sleeping} p
        #{poke sg_process_count, stopped} p
        #{poke sg_process_count, zombie} p

--
-- Foreign Calls
--

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_init :: IO CInt

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_snapshot :: IO CInt

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_shutdown :: IO CInt

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_drop_privileges :: IO CInt

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_set_error :: Error -> CChar -> IO ()

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_set_error_with_errno :: Error -> CChar -> IO ()

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_get_error :: IO Error

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_get_error_arg :: IO CChar

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_get_error_errno :: IO CInt

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_str_error :: Error -> IO CChar

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_get_host_info :: IO (Ptr HostInfo)

foreign import ccall safe "statgrab.h sg_get_cpu_stats"
    sg_get_cpu_stats :: IO (Ptr CpuStats)

foreign import ccall safe "statgrab.h sg_get_cpu_stats_diff"
    sg_get_cpu_stats_diff :: IO (Ptr CpuStats)

foreign import ccall safe "statgrab.h sg_get_cpu_percents"
    sg_get_cpu_stats_percents :: IO (Ptr CpuPercents)

foreign import ccall safe "statgrab.h sg_get_mem_stats"
    sg_get_mem_stats :: IO (Ptr MemStats)

foreign import ccall safe "statgrab.h sg_get_load_stats"
    sg_get_load_stats :: IO (Ptr LoadStats)

foreign import ccall safe "statgrab.h sg_get_user_stats"
    sg_get_user_stats :: IO (Ptr UserStats)

foreign import ccall safe "statgrab.h sg_get_disk_io_stats"
    sg_get_swap_stats :: IO (Ptr SwapStats)

foreign import ccall safe "statgrab.h sg_get_fs_stats"
    sg_get_fs_stats :: CInt -> IO (Ptr FsStats)

foreign import ccall safe "statgrab.h sg_get_disk_io_stats"
    sg_get_disk_io_stats :: CInt -> IO (Ptr DiskIOStats)

foreign import ccall safe "statgrab.h sg_get_disk_io_stats_diff"
    sg_get_disk_io_stats_diff :: CInt -> IO (Ptr DiskIOStats)

foreign import ccall safe "statgrab.h sg_get_network_io_stats"
    sg_get_network_io_stats :: CInt -> IO (Ptr NetworkIOStats)

foreign import ccall safe "statgrab.h sg_get_network_io_stats_diff"
    sg_get_network_io_stats_diff :: CInt -> IO (Ptr NetworkIOStats)

foreign import ccall safe "statgrab.h sg_get_network_iface_stats"
    sg_get_network_iface_stats :: CInt -> IO (Ptr NetworkIFaceStats)

foreign import ccall safe "statgrab.h sg_get_page_stats"
    sg_get_page_stats :: IO (Ptr PageStats)

foreign import ccall safe "statgrab.h sg_get_page_stats_diff"
    sg_get_page_stats_diff :: IO (Ptr PageStats)

foreign import ccall safe "statgrab.h sg_get_process_stats"
    sg_get_process_stats :: CInt -> IO (Ptr ProcessStats)

foreign import ccall safe "statgrab.h sg_get_process_count"
    sg_get_process_count :: IO (Ptr ProcessCount)
