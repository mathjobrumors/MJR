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

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE DerivingStrategies		#-}
{-# LANGUAGE StandaloneDeriving 	#-}
{-# LANGUAGE UndecidableInstances 	#-}
{-# LANGUAGE DataKinds 			    #-}
{-# LANGUAGE FlexibleInstances 		#-}


module Admin where

import Data.Text (concat, intercalate, pack, take, Text)
import Data.Time (getCurrentTime, secondsToDiffTime)
import Data.Int (Int64)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Web.ClientSession as C (encryptIO, decrypt)
import Data.Binary (encode, decode)
import Web.Cookie (SetCookie(..), defaultSetCookie, sameSiteLax, sameSiteStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Database.Persist.Sqlite (Entity(..),
       getBy, insert_, update, selectList,
       SelectOpt(..), (<.), (-=.), (==.), (=.),
       (>.), deleteWhere, deleteBy)

import Network.Socket (SockAddr(..), HostAddress)

import Text.StringRandom (stringRandomIO)
import Yesod (runDB, lookupSession, setSession, setCookie, liftIO, lookupCookie, getYesod)
import Yesod.Core (Html(..), HandlerFor(..), whamlet, notFound,
                   defaultLayout, invalidArgs,
                   toHtml, lookupPostParam)

import MainConfig
import ThreadForms (threadTimestamp)

---
--- Functionality for banning IP's
---

retrieveIp :: Post -> HandlerFor MathJobsApp (Maybe SockAddr)
retrieveIp post = do
    encryptedip <- runDB $ getBy $ UniqueThreadPostIp (postThreadId post) (postPostId post)
    MathJobsApp {..} <- getYesod
    case encryptedip of
        Just (Entity _ content) ->
            do
                let decrypted = C.decrypt randomKey (encodeUtf8 $ ipLogEncryptedIp content)
                case decrypted of
                    Just t ->
                        return $ Just $ (\x -> SockAddrInet 443 x) ((decode $ fromStrict $ t) :: HostAddress)
                    Nothing ->
                        return Nothing
        _ -> return Nothing

getIpInt :: Maybe SockAddr -> Int
getIpInt (Just (SockAddrInet port host)) = fromIntegral host :: Int
getIpInt _ = 0

----
--- Get Admin authorization
----

isAdmin :: Handler Bool
isAdmin = do
    MathJobsApp {..} <- getYesod
    admin <- lookupSession "adminauth"
    adminpersistent <- lookupCookie "admin"

    case (admin, C.decrypt key <$> (encodeUtf8 <$> adminpersistent)) of
        (Nothing, Nothing) -> do return False
        (Nothing, Just y) ->
            if (decode <$> (fromStrict <$> y)) `elem` (Just <$> authorizedTokens)
                then
                    do return True
                else
                    do return False
        (Just x, Nothing) ->
            if x `elem` authorizedTokens
                then do
                    -- Set a permanent cookie
                    value <- liftIO $ C.encryptIO key $ Data.ByteString.Lazy.toStrict (Data.Binary.encode x)
                    setCookie (cookie value)
                    return True
                else
                    return False
        (Just x, Just y) ->
            if x `elem` authorizedTokens
                then
                    if Just x /= (decode <$> (fromStrict <$> y)) then
                        do
                            value <- liftIO $ C.encryptIO key $ Data.ByteString.Lazy.toStrict (Data.Binary.encode x)
                            setCookie (cookie value)
                            return True
                    else do return True
                else
                    return False

    where

        cookie :: ByteString -> SetCookie
        cookie val = defaultSetCookie {
                setCookieName = encodeUtf8 $ pack "admin",
                setCookieValue = val,
                setCookieMaxAge = Just $ secondsToDiffTime (60*60*24*365*10),
                setCookieDomain = Nothing, -- Just $ encodeUtf8 $ pack "mathjobrumors.com"
                setCookieHttpOnly = True,
                setCookieSecure = True,
                setCookieSameSite = Just sameSiteStrict,
                setCookiePath = Just $ encodeUtf8 $ pack "/"
        }

getGetAuthR :: Text -> Handler Html
getGetAuthR v = do
    admin <- lookupSession "adminauth"
    case admin of
        Nothing -> do
            rand <- liftIO $ stringRandomIO $ pack "[a-z]{64}"
            setSession "adminauth" rand
            liftIO $ print $ Data.Text.concat ["Auth token associated to ", v, " is ", rand]
            notFound
        Just x -> do
            liftIO $ print $ Data.Text.concat ["Auth token associated to ", v, " is ", x]
            defaultLayout [whamlet|present|]
            notFound


----------------------------------------------------
-- Admin console. All the function below can only --
-- be executed if the session cookie contains a   --
-- valid token granted by the server              --
----------------------------------------------------

-- | Helper functions to convert between closely related
-- types Thread/SpamThread and Post/SpamPost.

-- N.B: Sticky argument is omitted

threadToSpam (Thread r1 r2 r3 r4 r5 r6 r7 r8 r9 r10) =
        SpamThread r1 r2 r3 r4 r5 r6 r7 r8 r9

postToSpam (Post r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12) =
    SpamPost r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12

-- | Verifies if admin privileges are granted
-- in the session cookies. If yes executes f.
-- If not return 404. 

verifyIfAdmin :: Handler Html -> Handler Html
verifyIfAdmin f = do
    admin <- isAdmin
    if admin then
        f
    else
        do notFound

-- | DELETE Handler 
-- | Deletes the thread with index v
-- and all associated posts

deleteDeleteThreadR :: Int -> Handler Html
deleteDeleteThreadR v = verifyIfAdmin $
    do
        runDB $ deleteWhere [PostThreadId ==. v]
        runDB $ deleteBy (UniqueThreadId v)
        maxid <- runDB $ getBy (UniqueIdent 1)
        case maxid of

            Just (Entity id content) -> do
                runDB $ update id [MaxIdNumThread -=. 1]
                defaultLayout ""

            Nothing -> invalidArgs ["unable to delete the thread"]

-- | PUT Handler 
-- | Puts the threads with index v into spam and
-- all associated posts. Then deletes the thread
-- and all associated posts. 

putSpamThreadR :: Int -> Handler Html
putSpamThreadR v = verifyIfAdmin $ do
    thread <- runDB $ getBy (UniqueThreadId v)
    case thread of
        Just (Entity _ t) -> do
            runDB $ insert_ $ threadToSpam t
            list <- runDB $ selectList [PostThreadId ==. v] [Desc PostPostId]
            putListIntoSpamThenDelete v list

        Nothing -> invalidArgs ["unable to move thread to spam"]
    where
        putListIntoSpamThenDelete v ((Entity id p):xz) = do
            runDB $ insert_ $ postToSpam p
            putListIntoSpamThenDelete v xz
        putListIntoSpamThenDelete v [] = do deleteDeleteThreadR v

-- | PUT Handler
-- | Put post l in thread v into spam and
-- then delete it. If there is only on post, 
-- move the thread to spam aswell and delete it.
-- Otherwise update the freshness of the thread. 

putSpamPostR :: Int -> Int -> Handler Html
putSpamPostR v l = verifyIfAdmin $ do
    thread <- runDB $ getBy (UniqueThreadId v)
    t <- runDB $ getBy (UniqueThreadPost v l)
    case (t, thread) of
        (Just (Entity _ p), Just (Entity _ t)) -> do
            runDB $ insert_ $ postToSpam p
            if threadNumPost t == 1 then
                do
                runDB $ insert_ $ threadToSpam t
                -- N.B: When there is only one post
                -- this also deletes the whole thread. 
                deleteDeletePostR v l >> defaultLayout ""
            else
                do
                deleteDeletePostR v l
                updatePostTime v >> defaultLayout ""

        _ -> invalidArgs ["unable to move post to spam"]

-- | DELETE Handler
-- | Delete post l in thread v.
-- If post is the only post in the thread
-- delete the whole thread. Otherwise update
-- the lastposttime of the thread. 

deleteDeletePostR :: Int -> Int -> Handler Html
deleteDeletePostR v l = verifyIfAdmin $
    do
        thread <- runDB $ getBy (UniqueThreadId v)
        post <- runDB $ getBy (UniqueThreadPost v l)
        case (thread, post) of
            (Just (Entity id t), Just (Entity _ p)) -> do
                if threadNumPost t == 1 then
                    -- N.B If only one post delete the whole thread
                    deleteDeleteThreadR v
                else do
                    let upvotes = postUpVotes p
                    let downvotes = postDownVotes p
                    runDB $ update id [ThreadNumPost -=. 1]
                    runDB $ update id [ThreadNumUpVotes -=. upvotes]
                    runDB $ update id [ThreadNumDownVotes -=. downvotes]
                    runDB $ deleteBy (UniqueThreadPost v l)
                    updatePostTime v >> defaultLayout "" -- update lastPostTime in thread

            _ -> invalidArgs ["unable to delete post"]

-- | Retrieve the title of thread v

getTitle :: Int -> Handler Text
getTitle v = do
    t <- runDB $ getBy $ UniqueThreadId v
    case t of
        Just (Entity _ stuff) ->
            return $ threadThreadTitle stuff
        Nothing -> return "[unable to retrieve thread title]"

-- | GET Handler
-- Generates HTML report of all posts that were reported. 

getListReportR :: Handler Html
getListReportR = verifyIfAdmin $ do
    list <- runDB $ selectList [PostReported ==. True] [Desc PostPostTime]
    contentlist <- Prelude.traverse extractContent list
    defaultLayout
        [whamlet|
$forall post <- contentlist
    <button onclick="fetch('@{DeletePostR (postThreadId $ fst post) (postPostId $ fst post)}', {method:'DELETE'}).then(() => {window.location.reload();});">Delete
    <button onclick="fetch('@{SpamPostR (postThreadId $ fst post) (postPostId $ fst post)}', {method:'PUT'}).then(() => {window.location.reload();});">Spam
    <button onclick="fetch('@{LetItGoR (postThreadId $ fst post) (postPostId $ fst post)}', {method:'PUT'}).then(() => {window.location.reload();});">Let it go
    <button onclick="fetch('@{DeleteThreadR (postThreadId $ fst post)}', {method:'DELETE'}).then(() => {window.location.reload();});">Delete Thread
    <button onclick="fetch('@{SpamThreadR (postThreadId $ fst post)}', {method:'PUT'}).then(() => {window.location.reload();});">Spam Thread
    Reported by #{length $ postReportedBy $ fst post}
    <p>
    Title : 
    <a href=@{ThreadR (postThreadId $ fst post) 1}>
        #{snd post}
    #{postContent $ fst post} 
   <hr>
                |]

    where
        extractContent (Entity _ content) =
            do
                title <- getTitle (postThreadId content)
                return (content, title)

-- | PUT Handler
-- Releases the report switch in database for 
-- thread v post l

putLetItGoR :: Int -> Int -> Handler Html
putLetItGoR v l = verifyIfAdmin $ do
        post <- runDB $ getBy $ UniqueThreadPost v l
        case post of
            Just (Entity id _) -> do
                runDB $ update id [PostReported =. False]
                runDB $ update id [PostReportedBy =. []]
                defaultLayout ""
            _ -> do invalidArgs ["failed"]

-- | IP stuff

getInspectR :: Int -> Handler Html
getInspectR host = verifyIfAdmin $ do
    inspectIp (SockAddrInet 443 (fromIntegral host :: Word32))

inspectIp :: SockAddr -> Handler Html
inspectIp v@(SockAddrInet _ ip) = do
    defaultLayout [whamlet|

    <form action=@{BlockR (fromIntegral ip)} method="POST">
        <div>
            <label for="say">
                Final message
            <input name="msg" id="say" value="">
        <div>
            <button>
                Ban IP
    
    <br>
    <br>
    |]

postBlockR :: Int -> Handler Html
postBlockR v = verifyIfAdmin $ do
    msg <- lookupPostParam "msg"
    case msg of
        Just txt ->
            do
                runDB $ insert_ $ BannedIp (SockAddrInet 443 (fromIntegral v :: Word32)) txt
                defaultLayout [whamlet|He got banned|]
        Nothing ->
            do
                runDB $ insert_ $ BannedIp (SockAddrInet 443 (fromIntegral v :: Word32)) (pack "")
                defaultLayout [whamlet|He got banned|]

------------------
-- Sage feature --
------------------

sage :: Int -> Int -> Handler ()
sage v p = do
    thread <- runDB $ getBy $ UniqueThreadId v
    post   <- runDB $ getBy $ UniqueThreadPost v p
    case (thread, post) of
        (Just (Entity tid threadcontent), Just (Entity pid postcontent)) ->
            let threadtime = threadLastPostTime threadcontent
                posttime   = postPostTime       postcontent
            in
                do
                    timebefore <- timeBeforePostTime v posttime
                    case timebefore of
                        Just t -> do
                            updateAfterPostTime v t
                            updatePostTime v
                            return ()
                        Nothing -> do
                            return ()

-- | Retrieves the most recent post in thread v and updates the 
-- "last post time" of the thread accordingly. 
-- 
-- N.B: This is used when deleting/moving to spam a post
-- from a thread and when saging an old thread. 

updatePostTime :: Int -> Handler ()
updatePostTime v = do
    mostRecent <- runDB $ selectList [PostThreadId ==. v] [Desc PostPostTime, LimitTo 1]
    thread <- runDB $ getBy (UniqueThreadId v)
    case (mostRecent, thread) of
        ([Entity _ post], Just (Entity idthread _)) -> do
            runDB $ update idthread [ThreadLastPostTime =. postPostTime post]
            runDB $ update idthread [ThreadTimestamp =. ThreadForms.threadTimestamp (postPostTime post) v]
            return ()
        _ -> do
            return ()

-- | Update all after posttime

updateAfterPostTime :: Int -> Int64 -> Handler ()
updateAfterPostTime v posttime = do
    after <- runDB $ selectList [PostThreadId ==. v, PostPostTime >. posttime] []

    sequence_ $ updateIt posttime <$> after

updateIt posttime (Entity _ content) = do
    updatePostTimeTo (postThreadId content) (postPostId content) posttime

updatePostTimeTo :: Int -> Int -> Int64 -> Handler ()
updatePostTimeTo v p posttime = do
    post <- runDB $ getBy $ UniqueThreadPost v p
    case post of
        Just (Entity id content) -> do
            runDB $ update id [PostPostTime =. posttime]
            return ()
        Nothing -> do
            return ()

-- | Returns the time of the previous post before post p 
-- in thread v

timeBeforePostTime :: Int -> Int64 -> Handler (Maybe Int64)
timeBeforePostTime v posttime = do
    mostrecent <- runDB $ selectList
        [PostThreadId ==. v, PostPostTime <. posttime]
        [Desc PostPostTime, LimitTo 1]

    case mostrecent of
        [Entity _ content] -> do return $ Just (postPostTime content)
        _ -> do return Nothing

postSageR :: Int -> Int -> Handler Html
postSageR v p = verifyIfAdmin $ do
    sage v p
    return $ toHtml $ pack ""
