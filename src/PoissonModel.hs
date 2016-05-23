{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module PoissonModel where


import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe( maybeToList )
import System.Random


import Common
import Interface
import Random
import User

import qualified GiaStation as ST
import qualified NoisyChannel as CH


type Delay = Int
type QMsg = (Delay, ForwMsg)
type PoissonModel = State PoissonModelState


data PoissonModelState = PoissonModelState {
        _numMessages    :: [Int],
        _pTransmit      :: Double,
        _stdGen         :: StdGen,

        _totalGenerated :: Int,
        _transmitted    :: Int,
        _delays         :: Int,
        _nConflicts     :: Int,

        _pendingMsgs    :: [QMsg],
        _activeMsgs     :: [QMsg],
        _activeUsers    :: [User],
        _conflictOver   :: Bool,
        _curConflictLen :: Int,
        _curConflictNum :: Int,

        _station        :: ST.Station,
        _channel        :: CH.Channel
    } deriving Show

makeLenses ''PoissonModelState


initPoissonModel :: [Int] -> StdGen -> [Double] -> Double -> PoissonModelState
initPoissonModel numGen boolGen noiseGen qs = PoissonModelState {
        _pTransmit      = 0.5,
        _numMessages    = numGen,
        _stdGen         = boolGen,

        _totalGenerated = 0,
        _transmitted    = 0,
        _delays         = 0,
        _nConflicts     = 0,

        _pendingMsgs    = [],
        _activeMsgs     = [],
        _activeUsers    = [],
        _conflictOver   = True,
        _curConflictLen = 0,
        _curConflictNum = 0,

        _station        = ST.initStation qs,
        _channel        = CH.initChannel noiseGen
    }


newMsg :: Int -> QMsg
newMsg = (0,) . ForwMsg . UID


tick :: QMsg -> QMsg
tick = _1 +~ 1


runPoissonModel :: Int -> PoissonModelState -> PoissonModelState
runPoissonModel nsteps = execState $ replicateM_ nsteps stepModel


stepModel :: PoissonModel ()
stepModel = do
    canSubmitNewMessages <- use conflictOver
    when canSubmitNewMessages startNewConflict

    (isOver, realWindow) <- stepConflictResolution

    conflictOver .= isOver
    when realWindow stepMessageGenerator
    when isOver stepStatistics
    curConflictLen += 1
    forceStats


stepStatistics :: PoissonModel ()
stepStatistics = do
    users <- use activeUsers
    let userDelays = users ^.. traversed.delay
        nConflict  = length userDelays
        sumDelays  = sum userDelays
    nConflicts  += 1
    delays      += sumDelays
    transmitted += nConflict
    curConflictLen .= 0


forceStats :: PoissonModel ()
forceStats = do
    !_delay <- use delays
    !_trans <- use transmitted
    !_gener <- use totalGenerated
    !_nconf <- use nConflicts
    return ()


stepMessageGenerator :: PoissonModel ()
stepMessageGenerator = do
    numNewMsgs <- roll numMessages
    total <- use totalGenerated
    totalGenerated += numNewMsgs
    -- stream of messages with UIDs from 0 to infty
    let newMessages = take numNewMsgs $ map newMsg [total + 1..]
    appendAll pendingMsgs newMessages
    pendingMsgs %= map tick


startNewConflict :: PoissonModel ()
startNewConflict = do
    pending <- use pendingMsgs
    pendingMsgs .= []
    activeMsgs .= pending

    users <- mapM newUser pending
    activeUsers .= users
    curConflictNum .= length users


newUser :: QMsg -> PoissonModel User
newUser qmsg = do
    bools <- boolsStream
    return $ initUser (qmsg, bools)


boolsStream :: PoissonModel [Bool]
boolsStream = do
    boolGen <- use stdGen
    p <- use pTransmit
    let (stream, newGen) = newBoolsStream p boolGen
    stdGen .= newGen
    return stream


stepConflictResolution :: PoissonModel (Bool, Bool)
stepConflictResolution = do
    (resp, fBack) <- stepUsersBefore >>= stepForwChannel >>= stepStation
    stepUsersAfter fBack
    return resp


stepUsersBefore :: PoissonModel [ForwMsg]
stepUsersBefore = zoom (activeUsers.traversed) $ maybeToList <$> stepUserBefore


stepForwChannel :: [ForwMsg] -> PoissonModel ForwSignal
stepForwChannel = zoom channel . CH.stepChannel


stepStation :: ForwSignal -> PoissonModel (ModelFeedback, StationFeedback)
stepStation = zoom station . ST.stepStation


stepUsersAfter :: StationFeedback -> PoissonModel ()
stepUsersAfter = zoom (activeUsers.traversed) . stepUserAfter
