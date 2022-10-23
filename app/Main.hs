{- This file is part of mathjobrumors.

   mathjobrumors is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation, either
   version 3 of the License, or (at your option) any later version.

   mathjobrumors is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with mathjobrumors. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

import Application
import Data.Time.Clock
import Database.Persist.Sqlite
import Data.Text
import Yesod
import Yesod.Core
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.Warp
import Network.Socket
import MainConfig
import Thread
import ThreadForms
import Data.Time (secondsToNominalDiffTime)
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Admin
import Yesod.Static
import Yesod.EmbeddedStatic
import Data.Time.Clock.System
import Captcha
import Auth
import System.IO
import SpamList
import Web.ClientSession as C
import Buttons
import Database.Persist.Class
import Tree
import Data.Set as Set (member, Set(..), unions, fromList, toList)
import Static 
import GetLast (getThreadLastR, getThreadLastUserR)
import GetAllowed (getAllowedR)
import PostTopic
import Text.StringRandom
import Control.Monad.State.Lazy
import Control.Concurrent.STM.TVar
import System.Posix.Signals
import Control.Monad.STM
import Control.Concurrent
import Network.Wai.Handler.Warp
import Control.Concurrent.STM
import Network.Wai (Request(..))
import Control.Concurrent.Async
import qualified Control.Exception as E
import Data.ByteString (ByteString(..))
import Data.Text.Encoding
import Data.Word (Word32)
import Data.Set
import qualified Data.Map.Strict as M
import Data.Int (Int64)
import SetupDB
import ExitCode
import RateLimitIP
import Redirect

mkYesodDispatch "MathJobsApp" resourcesMathJobsApp

ctrlC pool shared shutdownMVar sock a = do
    atomically $ putTMVar shutdownMVar ()
    return ()  

-- Kept in anticipation of daemonization 

getSocket max = 
    withSocketsDo $ do
        addr <- resolve
        open addr
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        Prelude.head <$> getAddrInfo (Just hints) Nothing (Just "443")

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock max
        return sock

warpSettings shutdownMVar activeConnections = 
    setOnOpen (connectionIncrement shutdownMVar activeConnections) 
        $ setOnClose (connectionDecrement activeConnections) 
        $ defaultSettings
        
    where

        connectionDecrement activeConnections _ = 
            atomically $ modifyTVar' activeConnections (subtract 1)

        connectionIncrement shutdownMVar activeConnections _ = 
            atomically $ do
                isEmpty <- isEmptyTMVar shutdownMVar
                modifyTVar' activeConnections (+1)
                return isEmpty

server :: Socket -> IO ()
server sock = do

    let conf = SqliteConf "database.sqlite" 10
    pool <- createPoolConfig conf
    timenow <- liftIO $ systemSeconds <$> getSystemTime

    key <- getDefaultKey

    shared <- newTVarIO (Data.Set.fromList [])
    activeConnections <- newTVarIO (0 :: Int)
    ratelimittvar <- newTVarIO M.empty

    randKey <- snd <$> C.randomKey 

    randomnonce <- liftIO $ stringRandomIO $ pack "[a-z]{16}"

    domainsfile <- readFile "domains.txt"
    allowedlist <- readFile "allowed.txt"
    stickylist <- readFile "sticky.txt"
    filecontents <- readFile "tokens.txt"

    let authorizedTokens = fmap pack $ Prelude.lines filecontents

    -- loads list of sticky threads in v
    -- loads list of authorizations into shared

    v <- setupDB stickylist timenow pool shared

    shutdownMVar <- newEmptyTMVarIO

    app <- toWaiApp MathJobsApp { persistConfig        = conf,
                                  connPool             = pool,
                                  getStatic            = staticdir,
                                  authorizedTokens     = authorizedTokens,
                                  key                  = key,
                                  domainsList          = listToTree $ toLower . pack <$> Prelude.lines domainsfile,
                                  allowedList          = extradomains allowedlist,
                                  stickyList           = v,
                                  MainConfig.randomKey = randKey,
                                  shared               = shared,
                                  randomNonce          = randomnonce,
                                  activeConnections    = activeConnections
                                }

    installHandler sigINT (CatchInfo (ctrlC pool shared shutdownMVar sock)) Nothing

    race_ 
        (atomically $ do takeTMVar shutdownMVar)
        (concurrently    
             (liftIO $ runTLSSocket defaultTlsSettings 
                (setPort 443 $ warpSettings shutdownMVar activeConnections) 
                sock ((rateLimitApplication randomnonce ratelimittvar) app))
             (liftIO $ runSettings (setPort 80 defaultSettings) redirectApp))

    saveTokensRemoveIp pool shared

    where

        extradomains a = 
                multiplyDomains $ 
                fromList $ 
                toLower . pack <$> Prelude.lines a
                
        multiplyDomains :: Set Text -> Set Text
        multiplyDomains v = Set.unions [v, fromList $ appendWWW <$> toList v]
            where
            appendWWW v = Data.Text.concat [pack "www.", v]
 

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering 
    sock <- getSocket 1024
    server sock