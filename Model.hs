{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Control.Arrow( (***) )
import Control.Monad.State.Strict
import Control.Lens

import Data.Maybe( maybeToList )

-- import System.Environment( getArgs )
import System.Random( newStdGen, split )

import User
import Interface( ForwMsg(..), MsgResult(..) )
import Common( append )
import Random( randomBools )

data ModelState = ModelState {
        _forwChannel :: ForwChannel,
        _backChannel :: BackChannel,
        _station     :: Station,
        _users       :: [User],
        _stats       :: Stats
    } deriving Show

data ForwChannel = ForwChannel
  deriving Show

data BackChannel = BackChannel
  deriving Show

data Station = Station
  deriving Show

data Stats = Stats {
        _history :: [MsgResult]
    } deriving Show

makeLenses ''Stats
makeLenses ''ModelState

type Model = State ModelState

initModel :: [User] -> ModelState
initModel usrs = ModelState {
        _forwChannel = ForwChannel,
        _backChannel = BackChannel,
        _station = Station,
        _users = usrs,
        _stats = Stats []
    }

runModel :: Int -> [User] -> ModelState
runModel nsteps usrs = execState steps $ initModel usrs
    where steps = replicateM_ nsteps stepModel

presentModel :: Int -> [UserParams] -> ModelState -> ModelState
presentModel nsteps userParams model = model & users .~ newUsers
    where newUsers = map cleanUser' uparams
          uparams = zip userParams usrs
          usrs = model ^. users
          cleanUser' = uncurry $ cleanUser nsteps

stepModel :: Model ()
stepModel = stepUsersBefore
        >>= stepForwChannel
        >>= stepStation
        >>= stepStatistics
        >>= stepBackChannel
        >>= stepUsersAfter

stepUsersBefore :: Model [ForwMsg]
stepUsersBefore = zoom (users.traversed) $ maybeToList <$> stepUserBefore

stepForwChannel :: [ForwMsg] -> Model [ForwMsg]
stepForwChannel = zoom forwChannel . return

stepStation :: [ForwMsg] -> Model MsgResult
stepStation = zoom station . stepStation'

stepStation' :: [ForwMsg] -> State Station MsgResult
stepStation' = return . recvMsgs

recvMsgs :: [ForwMsg] -> MsgResult
recvMsgs []  = Empty
recvMsgs [_] = Success
recvMsgs _   = Conflict

stepStatistics :: MsgResult -> Model MsgResult
stepStatistics = zoom stats . stepStats
    where stepStats res = append history res  -- history %= (res:)
                       >> return res

stepBackChannel :: MsgResult -> Model MsgResult
stepBackChannel = zoom backChannel . return

stepUsersAfter :: MsgResult -> Model ()
stepUsersAfter = zoom (users.traversed) . stepUserAfter

main :: IO ()
main = forM_ [0.01, 0.02 .. 1.0] $ \y -> do
--         nsteps <- read . head <$> getArgs
        gens <- map split <$> replicateM nusers newStdGen
        let userParams = map rstreams gens
            rstreams = randomBools y *** randomBools p
            usrs = map initUser userParams
            model = presentModel nsteps userParams $ runModel nsteps usrs
            mDelay = meanDelay $ model^.users
        putStrLn $ show y ++ "\t" ++ show (mDelay :: Double)
--         print $ model^.stats
--         mapM_ print $ model^.users
        return ()
    where nusers = 2
          nsteps = 100000
          p = 1.0 / fromIntegral nusers
