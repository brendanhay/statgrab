{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeFamilies             #-}

-- Module      : System.Statgrab.Base
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Bindings to the libstatgrab library.
module System.Statgrab.Base where

import Control.Applicative
import Data.ByteString          (packCString)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Statgrab.Internal

data family Struct a

-- | A wrapper around @Ptr a@ which keeps track of the result @Entries@,
-- needed for 'copyBatch'.
data PtrN a = PtrN
    { ptrEntries :: !Int
    , ptrUnwrap  :: Ptr a
    }

-- | Copy routines to marshall and unmarshall Storable @Stat a@ structures.
class Copy a where
    copyAt    :: Ptr  (Struct a) -> Int -> IO a
    copyBatch :: PtrN (Struct a) -> IO [a]
    copy      :: PtrN (Struct a) -> IO a

    copy      PtrN{..} = copyAt ptrUnwrap 0
    copyBatch PtrN{..} = mapM (\i -> copyAt ptrUnwrap i) entries
      where
        entries
            | ptrEntries > 1 = [0..ptrEntries - 1]
            | otherwise      = [0]

    {-# MINIMAL copyAt    #-}
    {-# INLINE  copy      #-}
    {-# INLINE  copyBatch #-}

-- | Bracket routines for acquiring and releasing @Ptr a@s.
class Stat a where
    acquire :: Entries -> IO (Ptr a)
    release :: Ptr a   -> IO Error

-- | A wrapper for 'acquire'. This allows tracking the number of @Entries@
-- contained in a @Ptr a@.
acquireN :: Stat a => IO (PtrN a)
acquireN = alloca $ \x -> PtrN <$> (fromIntegral <$> peek x) <*> acquire x
{-# INLINE acquireN #-}

releaseN :: Stat a => PtrN a -> IO Error
releaseN = release . ptrUnwrap
{-# INLINE releaseN #-}

type ErrorDetailsPtr     = Ptr ErrorDetails
type HostPtr             = Ptr (Struct Host)
type CPUPtr              = Ptr (Struct CPU)
type CPUPercentPtr       = Ptr (Struct CPUPercent)
type MemoryPtr           = Ptr (Struct Memory)
type LoadPtr             = Ptr (Struct Load)
type UserPtr             = Ptr (Struct User)
type SwapPtr             = Ptr (Struct Swap)
type FileSystemPtr       = Ptr (Struct FileSystem)
type DiskIOPtr           = Ptr (Struct DiskIO)
type NetworkIOPtr        = Ptr (Struct NetworkIO)
type NetworkInterfacePtr = Ptr (Struct NetworkInterface)
type PagePtr             = Ptr (Struct Page)
type ProcessPtr          = Ptr (Struct Process)
type ProcessCountPtr     = Ptr (Struct ProcessCount)

#include <statgrab.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

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

data ErrorDetails = ErrorDetails
    { erError :: !Error
    , erValue :: !CInt
    , erArg   :: !CString
    }

foreign import ccall safe "statgrab.h sg_get_error"
     sg_get_error :: IO Error

foreign import ccall safe "statgrab.h sg_get_error_arg"
     sg_get_error_arg :: IO CString

foreign import ccall safe "statgrab.h sg_get_error_errno"
     sg_get_error_errno :: IO CInt

foreign import ccall safe "statgrab.h sg_get_error_details"
     sg_get_error_details :: ErrorDetailsPtr -> IO Error

foreign import ccall safe "statgrab.h sg_str_error"
     sg_str_error :: Error -> IO CString

foreign import ccall safe "statgrab.h sg_strperror"
     sg_strperror :: Ptr CString -> ErrorDetailsPtr -> IO CString

foreign import ccall safe "statgrab.h sg_init"
     sg_init :: CInt -> IO Error

foreign import ccall safe "statgrab.h sg_snapshot"
     sg_snapshot :: IO Error

foreign import ccall safe "statgrab.h sg_shutdown"
     sg_shutdown :: IO Error

foreign import ccall safe "statgrab.h sg_drop_privileges"
     sg_drop_privileges :: IO Error

foreign import ccall safe "statgrab.h sg_get_nelements"
     sg_get_nelements :: Ptr () -> IO CSize

foreign import ccall safe "statgrab.h sg_free_stats_buf"
     sg_free_stats_buf :: Ptr () -> IO Error

foreign import ccall safe "statgrab.h sg_lock_mutex"
     sg_lock_mutex :: CString -> IO Error

foreign import ccall safe "statgrab.h sg_unlock_mutex"
     sg_unlock_mutex :: CString -> IO Error

#{enum HostState, HostState
    , stateUnknownConfig   = sg_unknown_configuration
    , statePhysicalHost    = sg_physical_host
    , stateVirtual         = sg_virtual_machine
    , stateParaVirtual     = sg_paravirtual_machine
    , stateHardwareVirtual = sg_hardware_virtualized
    }

data instance Struct Host = CHost
    { hostOsName    :: !CString
    , hostOsRelease :: !CString
    , hostOsVersion :: !CString
    , hostPlatform  :: !CString
    , hostName      :: !CString
    , hostBitWidth  :: !CUInt
    , hostState     :: !HostState
    , hostNCPU      :: !CUInt
    , hostMaxCPU    :: !CUInt
    , hostUptime    :: !CTime
    , hostSystime   :: !CTime
    }

instance Copy Host where
    copyAt ptr i = do
        CHost{..} <- peekElemOff ptr i
        Host <$> packCString hostOsName
             <*> packCString hostOsRelease
             <*> packCString hostOsVersion
             <*> packCString hostPlatform
             <*> packCString hostName
             <#> hostBitWidth
             <!> hostState
             <#> hostNCPU
             <#> hostMaxCPU
             <@> hostUptime
             <@> hostSystime

instance Storable (Struct Host) where
    alignment _ = #{alignment sg_host_info}
    sizeOf    _ = #{size      sg_host_info}

    peek p = CHost
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

    poke p CHost{..} = do
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

instance Stat (Struct Host) where
    acquire = sg_get_host_info_r
    release = sg_free_host_info

foreign import ccall safe "statgrab.h sg_get_host_info"
    sg_get_host_info :: Entries -> IO HostPtr

foreign import ccall safe "statgrab.h sg_get_host_info_r"
    sg_get_host_info_r :: Entries -> IO HostPtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_host_info :: HostPtr -> IO Error

data instance Struct CPU = CCPU
    { cpuUser                   :: !CLLong
    , cpuKernel                 :: !CLLong
    , cpuIdle                   :: !CLLong
    , cpuIOWait                 :: !CLLong
    , cpuSwap                   :: !CLLong
    , cpuNice                   :: !CLLong
    , cpuTotal                  :: !CLLong
    , cpuCtxSwitches            :: !CLLong
    , cpuVoluntaryCtxSwitches   :: !CLLong
    , cpuInvoluntaryCtxSwitches :: !CLLong
    , cpuSyscalls               :: !CLLong
    , cpuInterrupts             :: !CLLong
    , cpuSoftInterrupts         :: !CLLong
    , cpuSystime                :: !CTime
    }

instance Copy CPU where
    copyAt ptr i = do
        CCPU{..} <- peekElemOff ptr i
        CPU <%> cpuUser
            <#> cpuKernel
            <#> cpuIdle
            <#> cpuIOWait
            <#> cpuSwap
            <#> cpuNice
            <#> cpuTotal
            <#> cpuCtxSwitches
            <#> cpuVoluntaryCtxSwitches
            <#> cpuInvoluntaryCtxSwitches
            <#> cpuSyscalls
            <#> cpuInterrupts
            <#> cpuSoftInterrupts
            <@> cpuSystime

instance Storable (Struct CPU) where
    alignment _ = #{alignment sg_cpu_stats}
    sizeOf    _ = #{size      sg_cpu_stats}

    peek p = CCPU
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

    poke p CCPU{..} = do
        #{poke sg_cpu_stats, user} p cpuUser
        #{poke sg_cpu_stats, kernel} p cpuKernel
        #{poke sg_cpu_stats, idle} p cpuIdle
        #{poke sg_cpu_stats, iowait} p cpuIOWait
        #{poke sg_cpu_stats, swap} p cpuSwap
        #{poke sg_cpu_stats, nice} p cpuNice
        #{poke sg_cpu_stats, total} p cpuTotal
        #{poke sg_cpu_stats, context_switches} p cpuCtxSwitches
        #{poke sg_cpu_stats, voluntary_context_switches} p cpuVoluntaryCtxSwitches
        #{poke sg_cpu_stats, involuntary_context_switches} p cpuInvoluntaryCtxSwitches
        #{poke sg_cpu_stats, syscalls} p cpuSyscalls
        #{poke sg_cpu_stats, interrupts} p cpuInterrupts
        #{poke sg_cpu_stats, soft_interrupts} p cpuSoftInterrupts
        #{poke sg_cpu_stats, systime} p cpuSystime

instance Stat (Struct CPU) where
    acquire = sg_get_cpu_stats_r
    release = sg_free_cpu_stats

foreign import ccall safe "statgrab.h sg_get_cpu_stats"
    sg_get_cpu_stats :: Entries -> IO CPUPtr

foreign import ccall safe "statgrab.h sg_get_cpu_stats_diff"
    sg_get_cpu_stats_diff :: Entries -> IO CPUPtr

foreign import ccall safe "statgrab.h sg_get_cpu_stats_r"
    sg_get_cpu_stats_r :: Entries -> IO CPUPtr

foreign import ccall safe "statgrab.h sg_get_cpu_stats_diff_between"
    sg_get_cpu_stats_diff_between :: CPUPtr -> CPUPtr -> Entries -> IO CPUPtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_cpu_stats :: CPUPtr -> IO Error

data instance Struct CPUPercent = CCPUPercent
    { cpuPctUser      :: !CDouble
    , cpuPctKernel    :: !CDouble
    , cpuPctIdle      :: !CDouble
    , cpuPctIOWait    :: !CDouble
    , cpuPctSwap      :: !CDouble
    , cpuPctNice      :: !CDouble
    , cpuPctTimeTaken :: !CTime
    }

instance Copy CPUPercent where
    copyAt ptr i = do
        CCPUPercent{..} <- peekElemOff ptr i
        CPUPercent <$> pure (realToFrac cpuPctUser)
                   <@> cpuPctKernel
                   <@> cpuPctIdle
                   <@> cpuPctIOWait
                   <@> cpuPctSwap
                   <@> cpuPctNice
                   <@> cpuPctTimeTaken

instance Storable (Struct CPUPercent) where
    alignment _ = #{alignment sg_cpu_percents}
    sizeOf    _ = #{size      sg_cpu_percents}

    peek p = CCPUPercent
        <$> #{peek sg_cpu_percents, user} p
        <*> #{peek sg_cpu_percents, kernel} p
        <*> #{peek sg_cpu_percents, idle} p
        <*> #{peek sg_cpu_percents, iowait} p
        <*> #{peek sg_cpu_percents, swap} p
        <*> #{peek sg_cpu_percents, nice} p
        <*> #{peek sg_cpu_percents, time_taken} p

    poke p CCPUPercent{..} = do
        #{poke sg_cpu_percents, user} p cpuPctUser
        #{poke sg_cpu_percents, kernel} p cpuPctKernel
        #{poke sg_cpu_percents, idle} p cpuPctIdle
        #{poke sg_cpu_percents, iowait} p cpuPctIOWait
        #{poke sg_cpu_percents, swap} p cpuPctSwap
        #{poke sg_cpu_percents, nice} p cpuPctNice
        #{poke sg_cpu_percents, time_taken} p cpuPctTimeTaken

#{enum CPUPercentSource, CPUPercentSource
    , sourceEntireCCPU = sg_entire_cpu_percent
    , sourceDiffCCPU   = sg_last_diff_cpu_percent
    , sourceNewDiffCCPU= sg_new_diff_cpu_percent
}

foreign import ccall safe "statgrab.h sg_get_cpu_percents_of"
    sg_get_cpu_percents_of :: CPUPercentSource -> Entries -> IO CPUPercentPtr

foreign import ccall safe "statgrab.h sg_get_cpu_percents_r"
    sg_get_cpu_percents_r :: CPUPtr -> Entries -> IO CPUPercentPtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_cpu_percents :: CPUPercentPtr -> IO Error

data instance Struct Memory = CMemory
    { memTotal   :: !CULLong
    , memFree    :: !CULLong
    , memUsed    :: !CULLong
    , memCache   :: !CULLong
    , memSystime :: !CTime
    }

instance Copy Memory where
    copyAt ptr i = do
        CMemory{..} <- peekElemOff ptr i
        Memory <%> memTotal
               <#> memFree
               <#> memUsed
               <#> memCache
               <@> memSystime

instance Storable (Struct Memory) where
    alignment _ = #{alignment sg_mem_stats}
    sizeOf    _ = #{size      sg_mem_stats}

    peek p = CMemory
        <$> #{peek sg_mem_stats, total} p
        <*> #{peek sg_mem_stats, free} p
        <*> #{peek sg_mem_stats, used} p
        <*> #{peek sg_mem_stats, cache} p
        <*> #{peek sg_mem_stats, systime} p

    poke p CMemory{..} = do
        #{poke sg_mem_stats, total} p memTotal
        #{poke sg_mem_stats, free} p memFree
        #{poke sg_mem_stats, used} p memUsed
        #{poke sg_mem_stats, systime} p memSystime

instance Stat (Struct Memory) where
    acquire = sg_get_mem_stats_r
    release = sg_free_mem_stats

foreign import ccall safe "statgrab.h sg_get_mem_stats"
    sg_get_mem_stats :: Entries -> IO MemoryPtr

foreign import ccall safe "statgrab.h sg_get_mem_stats_r"
    sg_get_mem_stats_r :: Entries -> IO MemoryPtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_mem_stats :: MemoryPtr -> IO Error

data instance Struct Load = CLoad
    { load1       :: !CDouble
    , load5       :: !CDouble
    , load15      :: !CDouble
    , loadSystime :: !CTime
    }

instance Copy Load where
    copyAt ptr i = do
        CLoad{..} <- peekElemOff ptr i
        Load <$> pure (realToFrac load1)
             <@> load5
             <@> load15
             <@> loadSystime

instance Storable (Struct Load) where
    alignment _ = #{alignment sg_load_stats}
    sizeOf    _ = #{size      sg_load_stats}

    peek p = CLoad
        <$> #{peek sg_load_stats, min1} p
        <*> #{peek sg_load_stats, min5} p
        <*> #{peek sg_load_stats, min15} p
        <*> #{peek sg_load_stats, systime} p

    poke p CLoad{..} = do
        #{poke sg_load_stats, min1} p load1
        #{poke sg_load_stats, min5} p load5
        #{poke sg_load_stats, min15} p load15
        #{poke sg_load_stats, systime} p loadSystime

instance Stat (Struct Load) where
    acquire = sg_get_load_stats_r
    release = sg_free_load_stats

foreign import ccall safe "statgrab.h sg_get_load_stats"
    sg_get_load_stats :: Entries -> IO LoadPtr

foreign import ccall safe "statgrab.h sg_get_load_stats_r"
    sg_get_load_stats_r :: Entries -> IO LoadPtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_load_stats :: LoadPtr -> IO Error

data instance Struct User = CUser
    { userLoginName    :: !CString
    , userRecordId     :: !CString
    , userRecordIdSize :: !CSize
    , userDevice       :: !CString
    , userHostName     :: !CString
    , userPid          :: !CInt
    , userLoginTime    :: !CTime
    , userSystime      :: !CTime
    }

instance Copy User where
    copyAt ptr i = do
        CUser{..} <- peekElemOff ptr i
        User <$> packCString userLoginName
             <*> packCString userRecordId
             <#> userRecordIdSize
             <*> packCString userDevice
             <*> packCString userHostName
             <#> userPid
             <@> userLoginTime
             <@> userSystime

instance Storable (Struct User) where
    alignment _ = #{alignment sg_user_stats}
    sizeOf    _ = #{size      sg_user_stats}

    peek p = CUser
        <$> #{peek sg_user_stats, login_name} p
        <*> #{peek sg_user_stats, record_id} p
        <*> #{peek sg_user_stats, record_id_size} p
        <*> #{peek sg_user_stats, device} p
        <*> #{peek sg_user_stats, hostname} p
        <*> #{peek sg_user_stats, pid} p
        <*> #{peek sg_user_stats, login_time} p
        <*> #{peek sg_user_stats, systime} p

    poke p CUser{..} = do
        #{poke sg_user_stats, login_name} p userLoginTime
        #{poke sg_user_stats, record_id} p userRecordId
        #{poke sg_user_stats, record_id_size} p userRecordIdSize
        #{poke sg_user_stats, device} p userDevice
        #{poke sg_user_stats, hostname} p userHostName
        #{poke sg_user_stats, pid} p userPid
        #{poke sg_user_stats, login_time} p userLoginTime
        #{poke sg_user_stats, systime} p userSystime

instance Stat (Struct User) where
    acquire = sg_get_user_stats_r
    release = sg_free_user_stats

foreign import ccall safe "statgrab.h sg_get_user_stats"
    sg_get_user_stats :: Entries -> IO UserPtr

foreign import ccall safe "statgrab.h sg_get_user_stats_r"
    sg_get_user_stats_r :: Entries -> IO UserPtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_user_stats :: UserPtr -> IO Error

data instance Struct Swap = CSwap
    { swapTotal   :: !CULLong
    , swapUsed    :: !CULLong
    , swapFree    :: !CULLong
    , swapSystime :: !CTime
    }

instance Copy Swap where
    copyAt ptr i = do
        CSwap{..} <- peekElemOff ptr i
        Swap <%> swapTotal
             <#> swapUsed
             <#> swapFree
             <@> swapSystime

instance Storable (Struct Swap) where
    alignment _ = #{alignment sg_swap_stats}
    sizeOf    _ = #{size      sg_swap_stats}

    peek p = CSwap
        <$> #{peek sg_swap_stats, total} p
        <*> #{peek sg_swap_stats, used} p
        <*> #{peek sg_swap_stats, free} p
        <*> #{peek sg_swap_stats, systime} p

    poke p CSwap{..} = do
        #{poke sg_swap_stats, total} p swapTotal
        #{poke sg_swap_stats, used} p swapUsed
        #{poke sg_swap_stats, free} p swapFree
        #{poke sg_swap_stats, systime} p swapSystime

instance Stat (Struct Swap) where
    acquire = sg_get_swap_stats_r
    release = sg_free_swap_stats

foreign import ccall safe "statgrab.h sg_get_swap_stats"
    sg_get_swap_stats :: Entries -> IO SwapPtr

foreign import ccall safe "statgrab.h sg_get_swap_stats_r"
    sg_get_swap_stats_r :: Entries -> IO SwapPtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_swap_stats :: SwapPtr -> IO Error

#{enum DeviceType, DeviceType
    , deviceUnknown  = sg_fs_unknown
    , deviceRegular  = sg_fs_regular
    , deviceSpecial  = sg_fs_special
    , deviceLoopback = sg_fs_loopback
    , deviceRemote   = sg_fs_remote
    , deviceLocal    = sg_fs_local
    , deviceAllTypes = sg_fs_alltypes
    }

data instance Struct FileSystem = CFileSystem
    { fsDeviceName  :: !CString
    , fsType        :: !CString
    , fsMountPoint  :: !CString
    , fsDeviceType  :: !DeviceType
    , fsSize        :: !CULLong
    , fsUsed        :: !CULLong
    , fsFree        :: !CULLong
    , fsAvail       :: !CULLong
    , fsTotalInodes :: !CULLong
    , fsUsedInodes  :: !CULLong
    , fsFreeInodes  :: !CULLong
    , fsAvailInodes :: !CULLong
    , fsIOSize      :: !CULLong
    , fsBlockSize   :: !CULLong
    , fsTotalBlocks :: !CULLong
    , fsFreeBlocks  :: !CULLong
    , fsUsedBlocks  :: !CULLong
    , fsAvailBlocks :: !CULLong
    , fsSystime     :: !CTime
    }

instance Copy FileSystem where
    copyAt ptr i = do
        CFileSystem{..} <- peekElemOff ptr i
        FileSystem <$> packCString fsDeviceName
                   <*> packCString fsType
                   <*> packCString fsMountPoint
                   <!> fsDeviceType
                   <#> fsSize
                   <#> fsUsed
                   <#> fsFree
                   <#> fsAvail
                   <#> fsTotalInodes
                   <#> fsUsedInodes
                   <#> fsFreeInodes
                   <#> fsAvailInodes
                   <#> fsIOSize
                   <#> fsBlockSize
                   <#> fsTotalBlocks
                   <#> fsFreeBlocks
                   <#> fsUsedBlocks
                   <#> fsAvailBlocks
                   <@> fsSystime

instance Storable (Struct FileSystem) where
    alignment _ = #{alignment sg_fs_stats}
    sizeOf    _ = #{size      sg_fs_stats}

    peek p = CFileSystem
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

    poke p CFileSystem{..} = do
        #{poke sg_fs_stats, device_name} p fsDeviceName
        #{poke sg_fs_stats, fs_type} p fsType
        #{poke sg_fs_stats, mnt_point} p fsMountPoint
        #{poke sg_fs_stats, device_type} p fsDeviceType
        #{poke sg_fs_stats, size} p fsSize
        #{poke sg_fs_stats, used} p fsUsed
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

instance Stat (Struct FileSystem) where
    acquire = sg_get_fs_stats_r
    release = sg_free_fs_stats

foreign import ccall safe "statgrab.h sg_get_valid_filesystems"
     sg_get_valid_filesystems :: Entries -> IO (Ptr CString)

-- foreign import ccall safe "statgrab.h sg_get_fs_val"
--      sg_set_valid_filesystems :: Ptr CString -> IO Error
-- __sg_public const char **sg_get_valid_filesystems(size_t *entries);
-- __sg_public sg_error sg_set_valid_filesystems(const char *valid_fs[]);

foreign import ccall safe "statgrab.h sg_get_fs_stats"
     sg_get_fs_stats :: Entries -> IO FileSystemPtr

foreign import ccall safe "statgrab.h sg_get_fs_stats_r"
     sg_get_fs_stats_r :: Entries -> IO FileSystemPtr

foreign import ccall safe "statgrab.h sg_get_fs_stats_diff"
     sg_get_fs_stats_diff :: Entries -> IO FileSystemPtr

foreign import ccall safe "statgrab.h sg_get_fs_stats_diff_between"
     sg_get_fs_stats_diff_between :: FileSystemPtr
                                  -> FileSystemPtr
                                  -> Entries
                                  -> IO FileSystemPtr

foreign import ccall safe "statgrab.h sg_fs_compare_device_name"
     sg_fs_compare_device_name :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_fs_compare_mnt_point"
     sg_fs_compare_mnt_point :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_fs_stats :: FileSystemPtr -> IO Error

data instance Struct DiskIO = CDiskIO
    { diskName    :: !CString
    , diskRead    :: !CULLong
    , diskWrite   :: !CULLong
    , diskSystime :: !CTime
    }

instance Copy DiskIO where
    copyAt ptr i = do
        CDiskIO{..} <- peekElemOff ptr i
        DiskIO <$> packCString diskName
               <#> diskRead
               <#> diskWrite
               <@> diskSystime

instance Storable (Struct DiskIO) where
    alignment _ = #{alignment sg_disk_io_stats}
    sizeOf    _ = #{size      sg_disk_io_stats}

    peek p = CDiskIO
        <$> #{peek sg_disk_io_stats, disk_name} p
        <*> #{peek sg_disk_io_stats, read_bytes} p
        <*> #{peek sg_disk_io_stats, write_bytes} p
        <*> #{peek sg_disk_io_stats, systime} p

    poke p CDiskIO{..} = do
        #{poke sg_disk_io_stats, disk_name} p diskName
        #{poke sg_disk_io_stats, read_bytes} p diskRead
        #{poke sg_disk_io_stats, write_bytes} p diskWrite
        #{poke sg_disk_io_stats, systime} p diskSystime

instance Stat (Struct DiskIO) where
    acquire = sg_get_disk_io_stats_r
    release = sg_free_disk_io_stats

foreign import ccall safe "statgrab.h sg_get_disk_io_stats"
    sg_get_disk_io_stats :: Entries -> IO DiskIOPtr

foreign import ccall safe "statgrab.h sg_get_disk_io_stats_r"
    sg_get_disk_io_stats_r :: Entries -> IO DiskIOPtr

foreign import ccall safe "statgrab.h sg_get_disk_io_stats_diff"
    sg_get_disk_io_stats_diff :: Entries -> IO DiskIOPtr

foreign import ccall safe "statgrab.h sg_get_disk_io_stats_diff_between"
    sg_get_disk_io_stats_diff_between :: DiskIOPtr
                                      -> DiskIOPtr
                                      -> Entries
                                      -> IO DiskIOPtr

foreign import ccall safe "statgrab.h sg_disk_io_compare_name"
    sg_disk_io_compare_name :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_disk_io_compare_traffic"
    sg_disk_io_compare_traffic :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_disk_io_stats :: DiskIOPtr -> IO Error

data instance Struct NetworkIO = CNetworkIO
    { ifaceIOName     :: !CString
    , ifaceTX         :: !CULLong
    , ifaceRX         :: !CULLong
    , ifaceIPackets   :: !CULLong
    , ifaceOPackets   :: !CULLong
    , ifaceIErrors    :: !CULLong
    , ifaceOErrors    :: !CULLong
    , ifaceCollisions :: !CULLong
    , ifaceSystem     :: !CTime
    }

instance Copy NetworkIO where
    copyAt ptr i = do
        CNetworkIO{..} <- peekElemOff ptr i
        NetworkIO <$> packCString ifaceIOName
                  <#> ifaceTX
                  <#> ifaceRX
                  <#> ifaceIPackets
                  <#> ifaceOPackets
                  <#> ifaceIErrors
                  <#> ifaceOErrors
                  <#> ifaceCollisions
                  <@> ifaceSystem

instance Storable (Struct NetworkIO) where
    alignment _ = #{alignment sg_network_io_stats}
    sizeOf    _ = #{size      sg_network_io_stats}

    peek p = CNetworkIO
        <$> #{peek sg_network_io_stats, interface_name} p
        <*> #{peek sg_network_io_stats, tx} p
        <*> #{peek sg_network_io_stats, rx} p
        <*> #{peek sg_network_io_stats, ipackets} p
        <*> #{peek sg_network_io_stats, opackets} p
        <*> #{peek sg_network_io_stats, ierrors} p
        <*> #{peek sg_network_io_stats, oerrors} p
        <*> #{peek sg_network_io_stats, collisions} p
        <*> #{peek sg_network_io_stats, systime} p

    poke p CNetworkIO{..} = do
        #{poke sg_network_io_stats, interface_name} p ifaceIOName
        #{poke sg_network_io_stats, tx} p ifaceTX
        #{poke sg_network_io_stats, rx} p ifaceRX
        #{poke sg_network_io_stats, ipackets} p ifaceIPackets
        #{poke sg_network_io_stats, opackets} p ifaceOPackets
        #{poke sg_network_io_stats, ierrors} p ifaceIErrors
        #{poke sg_network_io_stats, oerrors} p ifaceOErrors
        #{poke sg_network_io_stats, collisions} p ifaceCollisions
        #{poke sg_network_io_stats, systime} p ifaceSystem

instance Stat (Struct NetworkIO) where
    acquire = sg_get_network_io_stats_r
    release = sg_free_network_io_stats

foreign import ccall safe "statgrab.h sg_get_network_io_stats"
     sg_get_network_io_stats :: Entries -> IO NetworkIOPtr

foreign import ccall safe "statgrab.h sg_get_network_io_stats_r"
     sg_get_network_io_stats_r :: Entries -> IO NetworkIOPtr

foreign import ccall safe "statgrab.h sg_get_network_io_stats_diff"
     sg_get_network_io_stats_diff :: Entries -> IO NetworkIOPtr

foreign import ccall safe "statgrab.h sg_get_network_io_stats_diff_between"
     sg_get_network_io_stats_diff_between :: NetworkIOPtr
                                          -> NetworkIOPtr
                                          -> Entries
                                          -> IO NetworkIOPtr

foreign import ccall safe "statgrab.h sg_network_io_compare_name"
     sg_network_io_compare_name :: Ptr () -> Ptr () -> IO (CInt)

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_network_io_stats :: NetworkIOPtr -> IO Error

#{enum InterfaceMode, InterfaceMode
    , duplexFull    = SG_IFACE_DUPLEX_FULL
    , duplexHalf    = SG_IFACE_DUPLEX_HALF
    , duplexUnknown = SG_IFACE_DUPLEX_UNKNOWN
    }

#{enum InterfaceStatus, InterfaceStatus
    , statusDown = SG_IFACE_DOWN
    , statusUp   = SG_IFACE_UP
    }

data instance Struct NetworkInterface = CNetworkInterface
    { ifaceName    :: !CString
    , ifaceSpeed   :: !CULLong
    , ifaceFactor  :: !CULLong
    , ifaceDuplex  :: !InterfaceMode
    , ifaceUp      :: !InterfaceStatus
    , ifaceSystime :: !CTime
    }

instance Copy NetworkInterface where
    copyAt ptr i = do
        CNetworkInterface{..} <- peekElemOff ptr i
        NetworkInterface <$> packCString ifaceName
                         <#> ifaceSpeed
                         <#> ifaceFactor
                         <!> ifaceDuplex
                         <!> ifaceUp
                         <@> ifaceSystime

instance Storable (Struct NetworkInterface) where
    alignment _ = #{alignment sg_network_iface_stats}
    sizeOf    _ = #{size      sg_network_iface_stats}

    peek p = CNetworkInterface
        <$> #{peek sg_network_iface_stats, interface_name} p
        <*> #{peek sg_network_iface_stats, speed} p
        <*> #{peek sg_network_iface_stats, factor} p
        <*> #{peek sg_network_iface_stats, duplex} p
        <*> #{peek sg_network_iface_stats, up} p
        <*> #{peek sg_network_iface_stats, systime} p

    poke p CNetworkInterface{..} = do
        #{poke sg_network_iface_stats, interface_name} p ifaceName
        #{poke sg_network_iface_stats, speed} p ifaceSpeed
        #{poke sg_network_iface_stats, factor} p ifaceFactor
        #{poke sg_network_iface_stats, duplex} p ifaceDuplex
        #{poke sg_network_iface_stats, up} p ifaceUp
        #{poke sg_network_iface_stats, systime} p ifaceSystime

instance Stat (Struct NetworkInterface) where
    acquire = sg_get_network_iface_stats_r
    release = sg_free_network_iface_stats

foreign import ccall safe "statgrab.h sg_get_network_iface_stats"
    sg_get_network_iface_stats :: Entries -> IO NetworkInterfacePtr

foreign import ccall safe "statgrab.h sg_get_network_iface_stats_r"
    sg_get_network_iface_stats_r :: Entries -> IO NetworkInterfacePtr

foreign import ccall safe "statgrab.h sg_network_iface_compare_name"
    sg_network_iface_compare_name :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_network_iface_stats :: NetworkInterfacePtr -> IO Error

data instance Struct Page = CPage
    { pagesIn      :: !CULLong
    , pagesOut     :: !CULLong
    , pagesSysTime :: !CTime
    }

instance Copy Page where
    copyAt ptr i = do
        CPage{..} <- peekElemOff ptr i
        Page <%> pagesIn
             <#> pagesOut
             <@> pagesSysTime

instance Storable (Struct Page) where
    alignment _ = #{alignment sg_page_stats}
    sizeOf    _ = #{size      sg_page_stats}

    peek p = CPage
        <$> #{peek sg_page_stats, pages_pagein} p
        <*> #{peek sg_page_stats, pages_pageout} p
        <*> #{peek sg_page_stats, systime} p

    poke p CPage{..} = do
        #{poke sg_page_stats, pages_pagein} p pagesIn
        #{poke sg_page_stats, pages_pageout} p pagesOut
        #{poke sg_page_stats, systime} p pagesSysTime

instance Stat (Struct Page) where
    acquire = sg_get_page_stats_r
    release = sg_free_page_stats

foreign import ccall safe "statgrab.h sg_get_page_stats"
     sg_get_page_stats :: Entries -> IO PagePtr

foreign import ccall safe "statgrab.h sg_get_page_stats_r"
     sg_get_page_stats_r :: Entries -> IO PagePtr

foreign import ccall safe "statgrab.h sg_get_page_stats_diff"
     sg_get_page_stats_diff :: Entries -> IO PagePtr

foreign import ccall safe "statgrab.h sg_get_page_stats_diff_between"
     sg_get_page_stats_diff_between :: PagePtr
                                    -> PagePtr
                                    -> Entries
                                    -> IO PagePtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_page_stats :: PagePtr -> IO Error

#{enum ProcessState, ProcessState
    , stateRunning  = SG_PROCESS_STATE_RUNNING
    , stateSleeping = SG_PROCESS_STATE_SLEEPING
    , stateStopped  = SG_PROCESS_STATE_STOPPED
    , stateZombie   = SG_PROCESS_STATE_ZOMBIE
    , stateUnknown  = SG_PROCESS_STATE_UNKNOWN
    }

data instance Struct Process = CProcess
    { procName        :: !CString
    , procTitle       :: !CString
    , procPid         :: !CInt
    , procParent      :: !CInt
    , procPGid        :: !CInt
    , procSessId      :: !CInt
    , procUid         :: !CUInt
    , procEUid        :: !CUInt
    , procGid         :: !CUInt
    , procEGid        :: !CUInt
    , procSwitches    :: !CULLong
    , procVoluntary   :: !CULLong
    , procInvoluntary :: !CULLong
    , procSize        :: !CULLong
    , procResident    :: !CULLong
    , procStart       :: !CTime
    , procSpent       :: !CTime
    , procCPUPercent  :: !CDouble
    , procNice        :: !CInt
    , procState       :: !ProcessState
    , procSystime     :: !CTime
    }

instance Copy Process where
    copyAt ptr i = do
        CProcess{..} <- peekElemOff ptr i
        Process <$> packCString procName
                <*> packCString procTitle
                <#> procPid
                <#> procParent
                <#> procPGid
                <#> procSessId
                <#> procUid
                <#> procEUid
                <#> procGid
                <#> procEGid
                <#> procSwitches
                <#> procVoluntary
                <#> procInvoluntary
                <#> procSize
                <#> procResident
                <@> procStart
                <@> procSpent
                <@> procCPUPercent
                <#> procNice
                <!> procState
                <@> procSystime

instance Storable (Struct Process) where
    alignment _ = #{alignment sg_process_stats}
    sizeOf    _ = #{size      sg_process_stats}

    peek p = CProcess
        <$> #{peek sg_process_stats, process_name} p
        <*> #{peek sg_process_stats, proctitle} p
        <*> #{peek sg_process_stats, pid} p
        <*> #{peek sg_process_stats, parent} p
        <*> #{peek sg_process_stats, pgid} p
        <*> #{peek sg_process_stats, sessid} p
        <*> #{peek sg_process_stats, uid} p
        <*> #{peek sg_process_stats, euid} p
        <*> #{peek sg_process_stats, gid} p
        <*> #{peek sg_process_stats, egid} p
        <*> #{peek sg_process_stats, context_switches} p
        <*> #{peek sg_process_stats, voluntary_context_switches} p
        <*> #{peek sg_process_stats, involuntary_context_switches} p
        <*> #{peek sg_process_stats, proc_size} p
        <*> #{peek sg_process_stats, proc_resident} p
        <*> #{peek sg_process_stats, start_time} p
        <*> #{peek sg_process_stats, time_spent} p
        <*> #{peek sg_process_stats, cpu_percent} p
        <*> #{peek sg_process_stats, nice} p
        <*> #{peek sg_process_stats, state} p
        <*> #{peek sg_process_stats, systime} p

    poke p CProcess{..} = do
        #{poke sg_process_stats, process_name} p procName
        #{poke sg_process_stats, proctitle} p procTitle
        #{poke sg_process_stats, pid} p procPid
        #{poke sg_process_stats, parent} p procParent
        #{poke sg_process_stats, pgid} p procPGid
        #{poke sg_process_stats, sessid} p procSessId
        #{poke sg_process_stats, uid} p procUid
        #{poke sg_process_stats, euid} p procEUid
        #{poke sg_process_stats, gid} p procGid
        #{poke sg_process_stats, egid} p procEGid
        #{poke sg_process_stats, context_switches} p procSwitches
        #{poke sg_process_stats, voluntary_context_switches} p procVoluntary
        #{poke sg_process_stats, involuntary_context_switches} p procInvoluntary
        #{poke sg_process_stats, proc_size} p procSize
        #{poke sg_process_stats, proc_resident} p procResident
        #{poke sg_process_stats, start_time} p procStart
        #{poke sg_process_stats, time_spent} p procSpent
        #{poke sg_process_stats, cpu_percent} p procCPUPercent
        #{poke sg_process_stats, nice} p procNice
        #{poke sg_process_stats, state} p procState
        #{poke sg_process_stats, systime} p procSystime

instance Stat (Struct Process) where
    acquire = sg_get_process_stats_r
    release = sg_free_process_stats

foreign import ccall safe "statgrab.h sg_get_process_stats"
    sg_get_process_stats :: Entries -> IO ProcessPtr

foreign import ccall safe "statgrab.h sg_get_process_stats_r"
    sg_get_process_stats_r :: Entries -> IO ProcessPtr

foreign import ccall safe "statgrab.h sg_process_compare_name"
    sg_process_compare_name :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_process_compare_pid"
    sg_process_compare_pid :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_process_compare_uid"
    sg_process_compare_uid :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_process_compare_gid"
    sg_process_compare_gid :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_process_compare_size"
    sg_process_compare_size :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_process_compare_res"
    sg_process_compare_res :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_process_compare_cpu"
    sg_process_compare_cpu :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_process_compare_time"
    sg_process_compare_time :: Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_process_stats :: ProcessPtr -> IO Error

#{enum ProcessSource, ProcessSource
    , sourceEntire = sg_entire_process_count
    , sourceLast   = sg_last_process_count
    }

data instance Struct ProcessCount = CProcessCount
    { countTotal    :: !CULLong
    , countRunning  :: !CULLong
    , countSleeping :: !CULLong
    , countStopped  :: !CULLong
    , countZombie   :: !CULLong
    , countUnknown  :: !CULLong
    , countSystime  :: !CTime
    }

instance Copy ProcessCount where
    copyAt ptr i = do
        CProcessCount{..} <- peekElemOff ptr i
        ProcessCount <%> countTotal
                     <#> countRunning
                     <#> countSleeping
                     <#> countStopped
                     <#> countZombie
                     <#> countUnknown
                     <@> countSystime

instance Storable (Struct ProcessCount) where
    alignment _ = #{alignment sg_process_count}
    sizeOf    _ = #{size      sg_process_count}

    peek p = CProcessCount
        <$> #{peek sg_process_count, total} p
        <*> #{peek sg_process_count, running} p
        <*> #{peek sg_process_count, sleeping} p
        <*> #{peek sg_process_count, stopped} p
        <*> #{peek sg_process_count, zombie} p
        <*> #{peek sg_process_count, unknown} p
        <*> #{peek sg_process_count, systime} p

    poke p CProcessCount{..} = do
        #{poke sg_process_count, total} p countTotal
        #{poke sg_process_count, running} p countRunning
        #{poke sg_process_count, sleeping} p countSleeping
        #{poke sg_process_count, stopped} p countStopped
        #{poke sg_process_count, zombie} p countZombie
        #{poke sg_process_count, unknown} p countUnknown
        #{poke sg_process_count, systime} p countSystime

foreign import ccall safe "statgrab.h sg_get_process_count_of"
    sg_get_process_count_of :: ProcessSource -> IO ProcessCountPtr

foreign import ccall safe "statgrab.h sg_get_process_count_r"
    sg_get_process_count_r :: ProcessPtr -> IO ProcessCountPtr

foreign import ccall safe "statgrab.h sg_free_stats_buf"
    sg_free_process_count :: ProcessCountPtr -> IO Error
