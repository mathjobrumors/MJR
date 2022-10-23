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

{-# LANGUAGE DerivingStrategies		    #-}
{-# LANGUAGE StandaloneDeriving 	    #-}
{-# LANGUAGE UndecidableInstances 	    #-}
{-# LANGUAGE DataKinds 		    	    #-}
{-# LANGUAGE FlexibleInstances 		    #-}
{-# LANGUAGE BlockArguments             #-}

module Thread where

import           Data.Text (Text, pack, take, concat, intercalate, replace, unpack)
import           Database.Persist.Sqlite
import           Yesod
import           Data.Int
import           Data.Time (secondsToDiffTime)
import           MainConfig
import           Text.Julius (rawJS)
import           Text.Hamlet (Html)
import           ThreadForms
import           Admin
import           Data.Time.Clock.System (systemSeconds, getSystemTime)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Text.Encoding (encodeUtf8)
import           Captcha (generateCaptchaUrl)
import           RateLimiting (rateLimiting, updateTimestamp)
import           MarkdownParsing (blockquoteMarkdown)
import           Admin (isAdmin)
import           NavigationBar (navigationBar)
import           Web.ClientSession as C (decrypt)
import           Data.Binary (decode)
import           Cookies (isPersistent, isPerThread, postCookieR, getFullUserName)
import           Captcha (captchaFrame)
import           Text.Blaze.Html (preEscapedToHtml)

import           Data.Set as Set (member, Set(..), unions, fromList, toList)
import           Static 
import           Network.Socket (SockAddr(..), HostAddress, hostAddressToTuple)
import           TimeFormat
import           ThreadJSHelper
import           GetAllowed
import           SinglePost
import           Data.IORef 
import           Auth (checkAuth)
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM

------------------------------
-- Default URL for threads ---
------------------------------

getThreadR :: Int -> Int -> Handler Text.Hamlet.Html
getThreadR v page   | page < 1 = do notFound
                    | otherwise = do

    thread <- runDB $ getBy (UniqueThreadId v)

    case thread of
        Nothing -> do notFound

        Just (Entity id threadData) -> do
            let postid = threadThreadId threadData
            let curThreadTitle = threadThreadTitle threadData
            let numpost = threadNumPost threadData

            hasauth <- checkAuth "timestamp"
            if hasauth then do
                runDB $ update id [ThreadNumViews +=. 1]
            else do
                return ()

            updateTimestamp

            posts <- runDB $ selectList [PostThreadId ==. v]
                                        [Asc PostPostId,
                                         LimitTo resultsPerThreadPage,
                                         OffsetBy $ (page - 1) * resultsPerThreadPage]
                                         
            request <- getRequest

            username <- getFullUserName v
            adminmode <- isAdmin
            render <- getUrlRender
            currentTime <- liftIO $ systemSeconds <$> getSystemTime

            let userconfig = UserPostData {
                val = username, 
                currenttime = currentTime,
                render = render 
            }

            let postconfig = defaultThreadPost { adminmode = adminmode }

            captchaurl <- generateCaptchaUrl

            threads <- runDB $ selectList []
                [ Desc ThreadTimestamp
                , LimitTo resultsPerPage
                , OffsetBy 0
                ]

            persistent <- isPersistent
            perThread <- isPerThread

            MathJobsApp {..} <- getYesod

            numconnections <- liftIO $ atomically $ readTVar activeConnections

            let numpages = lastPage threadData
            let iteratedposts = zip posts [1..]

            defaultLayout $ do
                    
                standardStylesheet

                addScript $ StaticR post_js
                addScriptRemote "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
                addScriptRemote "https://cdn.jsdelivr.net/npm/marked/marked.min.js"

                setTitle $ toHtml curThreadTitle

                headContent adminmode captchaurl v request allowedList

                toWidget $(whamletFile "post.html")


headContent adminmode captchaurl v request allowedlist = do

    toWidgetHead [lucius|
        html {overflow-x:hidden;}
        body {overflow-x:hidden; position: relative;}
    |]

    toWidgetHead [hamlet|<meta name="viewport" content="width=device-width, initial-scale=1.0">|]
    toWidgetHead [hamlet|<link rel="preload" as="image" href=@{captchaurl}>|]
    toWidgetHead [julius|
            var token_name=#{defaultCsrfParamName};
            var token=#{unMaybe $ reqToken request}; 
            var url='@{ThreadLastUserR v}';
        |]

    allowedListJS allowedlist
    
    if adminmode then do
        toWidgetHead [julius|
            function sagepost(url){
                if(confirm('Sage post?'))
                    fetch(url, {method: 'POST'}).then(() => {window.location.reload();});
            }
            function spampost(url){
                if(confirm('Move post to spam?'))
                    fetch(url, {method: 'PUT'}).then(() => {window.location.reload();});
            }
            function deletepost(url){
                if(confirm('Delete post?'))
                    fetch(url,{method:'DELETE'}).then(() => {window.location.reload();});
            }
        |]
    else do
        toWidgetHead [julius||]

unMaybe :: Maybe Text -> Text
unMaybe (Just x) = x
unMaybe Nothing = ""
