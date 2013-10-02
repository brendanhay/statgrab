{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : System.Statgrab.Interface
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module System.Statgrab.Interface where

import Control.Applicative
import Data.ByteString       (ByteString, packCString)
import Data.Time.Clock.POSIX
import Foreign
import System.Statgrab.Base

class Copy a where
    type Struct a
    copy :: Ptr (Struct a) -> IO a

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
    } deriving (Eq, Show)

instance Copy Host where
    type Struct Host = CHost
    copy ptr = do
        CHost{..} <- peek ptr
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
    } deriving (Eq, Show)

instance Copy CPU where
    type Struct CPU = CCPU
    copy ptr = do
        CCPU{..} <- peek ptr
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

data CPUPercent = CPUPercent
    { cpuPctUser      :: !Double
    , cpuPctKernel    :: !Double
    , cpuPctIdle      :: !Double
    , cpuPctIOWait    :: !Double
    , cpuPctSwap      :: !Double
    , cpuPctNice      :: !Double
    , cpuPctTimeTaken :: !POSIXTime
    } deriving (Eq, Show)

instance Copy CPUPercent where
    type Struct CPUPercent = CCPUPercent
    copy ptr = do
        CCPUPercent{..} <- peek ptr
        CPUPercent <$> pure (realToFrac cpuPctUser)
                   <@> cpuPctKernel
                   <@> cpuPctIdle
                   <@> cpuPctIOWait
                   <@> cpuPctSwap
                   <@> cpuPctNice
                   <@> cpuPctTimeTaken

data Memory = Memory
    { memTotal   :: !Integer
    , memFree    :: !Integer
    , memUsed    :: !Integer
    , memCache   :: !Integer
    , memSystime :: !POSIXTime
    } deriving (Eq, Show)

instance Copy Memory where
    type Struct Memory = CMemory
    copy ptr = do
        CMemory{..} <- peek ptr
        Memory <%> memTotal
               <#> memFree
               <#> memUsed
               <#> memCache
               <@> memSystime

data Load = Load
    { load1       :: !Double
    , load5       :: !Double
    , load15      :: !Double
    , loadSystime :: !POSIXTime
    } deriving (Eq, Show)

instance Copy Load where
    type Struct Load = CLoad
    copy ptr = do
        CLoad{..} <- peek ptr
        Load <$> pure (realToFrac load1)
             <@> load5
             <@> load15
             <@> loadSystime


data User = User
    { userLoginName    :: !ByteString
    , userRecordId     :: !ByteString
    , userRecordIdSize :: !Integer
    , userDevice       :: !ByteString
    , userHostName     :: !ByteString
    , userPid          :: !Integer
    , userLoginTime    :: !POSIXTime
    , userSystime      :: !POSIXTime
    } deriving (Eq, Show)

instance Copy User where
    type Struct User = CUser
    copy ptr = do
        CUser{..} <- peek ptr
        User <$> packCString userLoginName
             <*> packCString userRecordId
             <#> userRecordIdSize
             <*> packCString userDevice
             <*> packCString userHostName
             <#> userPid
             <@> userLoginTime
             <@> userSystime

data Swap = Swap
    { swapTotal   :: !Integer
    , swapUsed    :: !Integer
    , swapFree    :: !Integer
    , swapSystime :: !POSIXTime
    } deriving (Eq, Show)

instance Copy Swap where
    type Struct Swap = CSwap
    copy ptr = do
        CSwap{..} <- peek ptr
        Swap <%> swapTotal
             <#> swapUsed
             <#> swapFree
             <@> swapSystime

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
    } deriving (Eq, Show)

instance Copy FileSystem where
    type Struct FileSystem = CFileSystem
    copy ptr = do
        CFileSystem{..} <- peek ptr
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

data DiskIO = DiskIO
    { diskName    :: !ByteString
    , diskRead    :: !Integer
    , diskWrite   :: !Integer
    , diskSystime :: !POSIXTime
    } deriving (Eq, Show)

instance Copy DiskIO where
    type Struct DiskIO = CDiskIO
    copy ptr = do
        CDiskIO{..} <- peek ptr
        DiskIO <$> packCString diskName
               <#> diskRead
               <#> diskWrite
               <@> diskSystime

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
    } deriving (Eq, Show)

instance Copy NetworkIO where
    type Struct NetworkIO = CNetworkIO
    copy ptr = do
        CNetworkIO{..} <- peek ptr
        NetworkIO <$> packCString ifaceIOName
                  <#> ifaceTX
                  <#> ifaceRX
                  <#> ifaceIPackets
                  <#> ifaceOPackets
                  <#> ifaceIErrors
                  <#> ifaceOErrors
                  <#> ifaceCollisions
                  <@> ifaceSystem

data NetworkInterface = NetworkInterface
    { ifaceName    :: !ByteString
    , ifaceSpeed   :: !Integer
    , ifaceFactor  :: !Integer
    , ifaceDuplex  :: !InterfaceMode
    , ifaceUp      :: !InterfaceStatus
    , ifaceSystime :: !POSIXTime
    } deriving (Eq, Show)

instance Copy NetworkInterface where
    type Struct NetworkInterface = CNetworkInterface
    copy ptr = do
        CNetworkInterface{..} <- peek ptr
        NetworkInterface <$> packCString ifaceName
                         <#> ifaceSpeed
                         <#> ifaceFactor
                         <!> ifaceDuplex
                         <!> ifaceUp
                         <@> ifaceSystime

data Page = Page
    { pagesIn      :: !Integer
    , pagesOut     :: !Integer
    , pagesSysTime :: !POSIXTime
    } deriving (Eq, Show)

instance Copy Page where
    type Struct Page = CPage
    copy ptr = do
        CPage{..} <- peek ptr
        Page <%> pagesIn
             <#> pagesOut
             <@> pagesSysTime

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
    } deriving (Eq, Show)

instance Copy Process where
    type Struct Process = CProcess
    copy ptr = do
        CProcess{..} <- peek ptr
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

data ProcessCount = ProcessCount
    { countTotal    :: !Integer
    , countRunning  :: !Integer
    , countSleeping :: !Integer
    , countStopped  :: !Integer
    , countZombie   :: !Integer
    , countUnknown  :: !Integer
    , countSystime  :: !POSIXTime
    } deriving (Eq, Show)

instance Copy ProcessCount where
    type Struct ProcessCount = CProcessCount
    copy ptr = do
        CProcessCount{..} <- peek ptr
        ProcessCount <%> countTotal
                     <#> countRunning
                     <#> countSleeping
                     <#> countStopped
                     <#> countZombie
                     <#> countUnknown
                     <@> countSystime

infixl 4 <%>, <#>, <@>, <!>

(<%>) :: (Integral a, Applicative f) => (Integer -> b) -> a -> f b
(<%>) a b = a <$> pure (toInteger b)

(<#>) :: (Integral a, Applicative f) => f (Integer -> b) -> a -> f b
(<#>) a b = a <*> pure (toInteger b)

(<@>) :: (Fractional a, Real c, Applicative f) => f (a -> b) -> c -> f b
(<@>) a b = a <*> pure (realToFrac b)

(<!>) :: Applicative f => f (a -> b) -> a -> f b
(<!>) a b = a <*> pure b
