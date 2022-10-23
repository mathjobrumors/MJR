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

{-# LANGUAGE RecordWildCards			    #-}
{-# LANGUAGE EmptyDataDecls             	#-}
{-# LANGUAGE FlexibleContexts           	#-}
{-# LANGUAGE GADTs                      	#-}
{-# LANGUAGE GeneralizedNewtypeDeriving 	#-}
{-# LANGUAGE MultiParamTypeClasses      	#-}
{-# LANGUAGE OverloadedStrings          	#-}
{-# LANGUAGE QuasiQuotes                	#-}
{-# LANGUAGE TemplateHaskell            	#-}
{-# LANGUAGE TypeFamilies               	#-}
{-# LANGUAGE ViewPatterns               	#-}
{-# LANGUAGE DerivingStrategies		    	#-}
{-# LANGUAGE StandaloneDeriving 	    	#-}
{-# LANGUAGE UndecidableInstances 	    	#-}
{-# LANGUAGE DataKinds 			    	    #-}
{-# LANGUAGE FlexibleInstances 		    	#-}

module Application where

import Crypto.Hash.SHA256 as SHA256 (hash)

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (concat, intercalate, pack, unpack, take, toLower, Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (secondsToDiffTime)
import Data.Time.Clock.System (systemSeconds, getSystemTime)
import Network.Wai.Internal (Request(..))
import Text.Julius  (julius, rawJS)
import Database.Persist.Sqlite (Entity(..),
       getBy, selectList, insert_, update,
       SelectOpt(..), (<.), (-=.), (==.), (=.),
       (>.))

import Yesod (Entity(..), runDB)
import Yesod.Core

import Admin (isAdmin)
import Auth (successMsg, dropError)
import Captcha (generateCaptchaUrl)
import MainConfig
import NavigationBar (navigationBar)
import MarkdownParsing (text2Html)
import RateLimiting (updateTimestamp, rateLimiting)
import ThreadForms (postProtection, encryptIp, maxMsgSize, threadTimestamp)
import TextParsing (isAllowed)
import Cookies (isPerThread, isPersistent, postCookieR, getFullUserName)
import Captcha (captchaFrame)
import           Data.Set as Set (member, Set(..), unions, fromList, toList)
import Static (standardStylesheet, defaultLink, resultsPerPage, resultsPerThreadPage, altClass)
import TimeFormat
import Tree
import TextValidate
import Text.Blaze.Internal
import GetAllowed (allowedListJS)
import SinglePost
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

getHomeR :: Handler Html
getHomeR = do
    getPageR 1

-- | GET Handler
-- Output page v of the listing of all threads

getPageR :: Int -> Handler Html
getPageR v | v <= 0 = notFound
           | otherwise = do            

    updateTimestamp -- rate limiting timestamp

    MathJobsApp {..} <- getYesod

    numconnections <- liftIO $ atomically $ readTVar activeConnections

    threads <- 
        if v == 1 then do

            nonSticky <- runDB $ selectList [ThreadSticky ==. False]
                [ Desc ThreadTimestamp
                , LimitTo (resultsPerPage - (length stickyList))
                , OffsetBy 0 ] 

            stickyNewList <- runDB $ selectList [ThreadSticky ==. True]
                [ Desc ThreadTimestamp ]
            
            return ((changeTitle <$> stickyNewList) ++ nonSticky)
                
        else do
            
            runDB $ selectList [ThreadSticky ==. False]
                [ Desc ThreadTimestamp
                , LimitTo resultsPerPage
                , OffsetBy $ (v - 1) * resultsPerPage - (length stickyList) ]

    request <- getRequest -- used by main.html for CSRF

    numThreads <- runDB $ getBy $ UniqueIdent 1
    currentTime <- liftIO $ systemSeconds <$> getSystemTime
    persistent <- isPersistent
    perThread <- isPerThread

    username <- getFullUserName v
    render <- getUrlRender

    let userconfig = UserPostData {
        val = username, 
        currenttime = currentTime,
        render = render 
    }

    case numThreads of
        Just (Entity _ g) -> do

            -- N.B. This actually depends on the starting point of the database.
            -- The -1 is if the starting point was one. So be careful here. 

            let maxThreadsPages = totalPagesHome ((maxIdNumThread g) - 1)

            captchaurl <- generateCaptchaUrl
            adminmode <- isAdmin

            defaultLayout $ do

                setTitle $ toHtml $ pack "Math Job Rumors"

                standardStylesheet

                addScript $ StaticR home_js

                addScriptRemote "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
                addScriptRemote "https://cdn.jsdelivr.net/npm/marked/marked.min.js"

                headContent adminmode allowedList captchaurl

                let iterthreads = zip threads [1..]
                toWidget $(whamletFile "main.html")

        _ -> invalidArgs ["unable to retrieve global id; catastrophic failure"]

    where

            totalPagesHome num =
                case num `mod` resultsPerPage of
                    0 -> div num resultsPerPage
                    _ -> 1 + div num resultsPerPage

            unMaybe :: Maybe Text -> Text
            unMaybe (Just x) = x
            unMaybe Nothing = ""

            changeTitle (Entity id content) = 
                Entity id content { 
                    threadThreadTitle = Data.Text.concat [pack "\128204", threadThreadTitle content]
                }

            defaultLinkAlt thread res currentTime adminmode = let
                threadid = fromIntegral (threadThreadId thread)
                threadtitle = threadThreadTitle thread
                threadnumpost = threadNumPost thread
                in
                [whamlet|
                        <td style="width:100%">
                            ^{defaultLink thread}
                        <td class="num poor" style="text-align: center">
                            #{threadNumPost thread}   
                        <td class="num" style="text-align: center">
                            #{threadNumViews thread}
                        <td class="num" style="text-align: center" class="notd">    
                            #{threadNumUpVotes thread}-#{threadNumDownVotes thread}
                        <td class="num" style="text-align: center">                    
                            <div class="full-text">
                                #{formatTime Long (currentTime - (threadLastPostTime thread))}
                            <div class="short-text">
                                #{formatTime Short (currentTime - (threadLastPostTime thread))}
                            
                        $if adminmode 
                            <td>
                                <input type="submit" class="textinput" onClick="deletethread('@{DeleteThreadR (threadThreadId thread)}')" value="Delete">
                            <td> 
                                <input type="submit" class="textinput" onClick="spamthread('@{SpamThreadR (threadThreadId thread)}')" value="Spam">
                    |]

headContent adminmode allowedlist captchaurl = do

    toWidgetHead [hamlet|<meta name="viewport" content="width=device-width">|]
    toWidgetHead [hamlet|<link rel="preload" as="image" href=@{captchaurl}>|]
    toWidgetHead [hamlet|<meta name="description" content="An anonymous forum for mathematicians">|]
    
    allowedListJS allowedlist

    if adminmode then do
        toWidgetHead [julius|
            function deletethread(url){
                if(confirm('Confirm deletion?'))
                    fetch(url, {method: 'DELETE'}).then(() => {window.location.reload();});
            }

            function spamthread(url){
                if(confirm('Confirm moving to spam?'))
                    fetch(url, {method: 'PUT'}).then(() => {window.location.reload();});
            }
            |]
    else do
        toWidgetHead [julius||]
