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

{-# LANGUAGE EmptyDataDecls			        #-}
{-# LANGUAGE FlexibleContexts           	#-}
{-# LANGUAGE GADTs                      	#-}
{-# LANGUAGE GeneralizedNewtypeDeriving 	#-}
{-# LANGUAGE MultiParamTypeClasses      	#-}
{-# LANGUAGE OverloadedStrings          	#-}
{-# LANGUAGE QuasiQuotes                	#-}
{-# LANGUAGE TypeFamilies               	#-}
{-# LANGUAGE ViewPatterns               	#-}
{-# LANGUAGE RecordWildCards            	#-}
{-# LANGUAGE DerivingStrategies		    	#-}
{-# LANGUAGE StandaloneDeriving 	    	#-}
{-# LANGUAGE UndecidableInstances 	    	#-}
{-# LANGUAGE DataKinds 			            #-}
{-# LANGUAGE FlexibleInstances 		    	#-}

module Auth where

import Data.Binary (decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Text (concat, intercalate, pack, take, unpack, Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (secondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemSeconds)

import Network.HTTP.Types (status400)
import Text.StringRandom (stringRandomIO)

import Web.ClientSession as C (decrypt, encryptIO)
import Web.Cookie (SetCookie(..), defaultSetCookie, sameSiteStrict)
import Yesod (Entity(..), runDB, getBy, delete, insert_, deleteBy)
import Yesod.Core (Html(..), whamlet, defaultLayout, liftIO, toHtml)
import Yesod.Core.Handler (getYesod, lookupSession, lookupSessionBS, lookupCookie,
                           setCookie, setSession, setSessionBS, notFound,
                           deleteSession, sendResponseStatus, waiRequest)

import Network.Wai.Internal
import MainConfig
import Data.Int
import Data.IORef
import Data.Set
import System.Random
import Cookies
import Network.Socket
import Data.ByteString (take)
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad (forM_)

getIpInt :: Maybe SockAddr -> Int
getIpInt (Just (SockAddrInet port host)) = fromIntegral host :: Int
getIpInt _ = 0

----
--- Get Captcha authorization token
---- 

authId :: Text -> Text
authId t = Data.Text.concat [pack "newauth_", t]

grantAuth :: Text -> Handler Text
grantAuth t = do
    auth <- lookupSessionBS $ authId t
    MathJobsApp {..} <- getYesod
    case (decode <$> fromStrict <$> auth :: Maybe Authorization) of
        Just x -> do
            liftIO $ atomically $ modifyTVar' shared (\a -> Data.Set.delete x a)
            generateAccess t
        Nothing -> do generateAccess t

    where

        generateAccess :: Text -> Handler Text
        generateAccess t = do

            MathJobsApp {..} <- getYesod
            time <- liftIO getSystemTime
            request <- waiRequest

            let token = Data.Text.take 8 $ sha256sumAscii $ Data.Text.concat 
                            [pack $ show $ getIpInt $ Just $ remoteHost request, 
                             pack $ show $ systemSeconds time,
                             randomNonce,
                             t] 

            let auth = Authorization { id = token, 
                                    timestamp = systemSeconds time } 
                                    
            setSessionBS (authId t) $ toStrict $ encode $ auth

            databaseSize <- liftIO $ atomically $ stateTVar shared (\s -> (size s, s)) 

            if databaseSize > 128*1024 then do 
                liftIO $ atomically $ modifyTVar' shared 
                    (\a -> Data.Set.insert auth $ Data.Set.deleteMin a)
            else do 
                liftIO $ atomically $ modifyTVar' shared 
                    (\a -> Data.Set.insert auth a)

            return $ token

-- Get Auth token. If found, then refresh it. 
-- So that it's marked as recently used and less
-- likely to be deleted. 

getAuthToken :: Text -> Handler Text
getAuthToken t = do 
    valid <- checkAuth t
    if valid then do
        auth <- lookupSessionBS $ authId t
        case (decode <$> fromStrict <$> auth :: Maybe Authorization) of
            Just x -> do             
                updateTimestamp x t
                return $ (MainConfig.id x)
            Nothing -> grantAuth t
    else do
        grantAuth t

updateTimestamp auth t = do

    time <- liftIO getSystemTime
    MathJobsApp {..} <- getYesod

    -- Update timestamp
    let newauth = auth { timestamp = systemSeconds time }

    liftIO $ atomically $ modifyTVar' shared 
        (\a -> Data.Set.insert newauth $ Data.Set.delete auth a)

    setSessionBS (authId t) $ toStrict $ encode $ newauth

    return ()

checkAuth :: Text -> Handler Bool
checkAuth t = do
    auth <- lookupSessionBS (authId t)
    case (decode <$> fromStrict <$> auth :: Maybe Authorization) of
        Just x -> do

            MathJobsApp {..} <- getYesod
            databaseMember <- liftIO $ atomically $ 
                    stateTVar shared (\s -> (member x s, s))

            if databaseMember then do
                updateTimestamp x t
                return True
            else do
                return False

        Nothing -> do return False

dropError :: Text -> Text -> Handler Html
dropError id txt = do
    html <- defaultLayout [whamlet|
        <body onload="alert('#{toHtml txt}');">
            <div id=#{id}>
                #{toHtml txt}
    |]
    sendResponseStatus status400 html


successMsg :: Text -> Text -> Handler Html
successMsg id txt = do
    defaultLayout [whamlet|
            <div id=#{id}>
                #{toHtml txt}
    |]
