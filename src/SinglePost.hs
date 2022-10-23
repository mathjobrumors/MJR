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

module SinglePost where

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

data PostConfig = PostConfig {
    
    vote :: Text -> Post
                 -> (Int -> Int -> Route MathJobsApp)
                 -> (Route MathJobsApp -> Text) -> Text,

    report :: Text -> Post
                 -> (Int -> Int -> Route MathJobsApp)
                 -> (Route MathJobsApp -> Text) -> Text,

    makeId :: Text -> Text,
    quote :: Int -> Text,
    adminmode :: Bool
}

data UserPostData = UserPostData {
    render :: Route MathJobsApp -> Text, 
    val :: Text,
    currenttime :: Int64
}

defaultThreadPost = PostConfig {
    vote = voteJS, 
    report = reportJS, 
    makeId = Data.Text.take 4,
    quote = (\x -> Data.Text.concat ["quote('", postId x, "')"]),
    adminmode = False
}

defaultPreviewPost = PostConfig {
    vote = (\_ _ _ _ -> pack ""),
    report = (\_ _ _ _ -> pack ""),
    makeId = (\x -> x),
    quote = (\x -> pack ""),
    adminmode = False
}

postPreview currenttime val = Post {
    postThreadId = 0, 
    postPostId = 1,
    postAuthorCookieId = pack "Preview",
    postContent = preEscapedToHtml $ pack "<div id=\"previewarea\"><div style=\"opacity:0.0\">N</div></div>",
    postUpVotes = 0,
    postDownVotes = 0,
    postUpVotedBy = [], 
    postDownVotedBy = [],
    postReported = False, 
    postReportedBy = [], 
    postMarkdownContent = pack "", 
    postPostTime = currenttime
}

defaultSpamPost = defaultPreviewPost

postEntryCustom :: PostConfig -> UserPostData -> Int -> Post -> WidgetFor MathJobsApp ()
postEntryCustom postconfig u v post = do

    ip <- handlerToWidget $ retrieveIp post

    [whamlet|

        <div id=#{postId (postPostId post)} data-value=#{blockquoteMarkdown $ postMarkdownContent $ post}>
        
            <div class="threadauthor">    
                    <strong>
                        Top Mathematician
                    <br>
                    <small>
                        #{toHtml $ (makeId postconfig) $ postAuthorCookieId post}  
                $if adminmode postconfig
                    $if ip /= Nothing
                        <br>
                        <a href=@{InspectR $ getIpInt ip}>
                            Ban IP
                    <br>
                    <br>
                    <button onclick="sagepost('@{SageR v (postPostId post)}');">
                        Sage
                    <br>
                    <br>
                    <button onclick="spampost('@{SpamPostR (postThreadId post) (postPostId post)}');">
                        Spam
                    <br>
                    <button onclick="deletepost('@{DeletePostR (postThreadId post) (postPostId post)}');">
                        Delete

            <div class="threadpost">
                <div class="post">
                    #{postContent post}
            
                <div class="poststuff">
                    #{formatTime Long ((currenttime u) - (postPostTime post))}
                    <span class="short-text rb">
                        #{toHtml $ makeId postconfig $ postAuthorCookieId post}
                    
                    <br class="short-text">

                    <a onclick="#{preEscapedToHtml $ (quote postconfig) (postPostId post)}">
                        Quote

                    $if (elem (val u) (postUpVotedBy post))
                        <span id="#{tokenize $ VUpR v (postPostId post)}" data-voted=1> 
                            <b>
                                #{toHtml $ postUpVotes post}
                    $else
                        <span id="#{tokenize $ VUpR v (postPostId post)}" data-voted=0>
                            #{toHtml $ postUpVotes post}
            
                    <a onclick=#{preEscapedToHtml $ (vote postconfig) (val u) post VUpR (render u)}> 
                        Up

                    $if (elem (val u) (postDownVotedBy post))
                        <span id="#{tokenize $ VDownR v (postPostId post)}" data-voted=1>
                            <b>
                                #{toHtml $ postDownVotes post}
                    $else
                        <span id="#{tokenize $ VDownR v (postPostId post)}" data-voted=0>
                            #{toHtml $ postDownVotes post}
            
                    <a onclick=#{preEscapedToHtml $ (vote postconfig) (val u) post VDownR (render u)}> 
                        Down

                    <a onclick=#{preEscapedToHtml $ (report postconfig) (val u) post ReportR (render u)} class="rb">
                        Report
                        
            |]
