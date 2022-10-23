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

{-# LANGUAGE RecordWildCards		#-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DerivingStrategies		#-}
{-# LANGUAGE UndecidableInstances 	#-}
{-# LANGUAGE DataKinds 			    #-}
{-# LANGUAGE FlexibleInstances 		#-}

module ThreadForms where

import		     Yesod
import           Data.Text (Text, pack, take, concat, intercalate, unpack, singleton, toLower)
import           Database.Persist.Sqlite
import           Data.Time.Clock (UTCTime(..), nominalDiffTimeToSeconds)
import           Data.Int (Int64)
import           Text.StringRandom (stringRandomIO)
import           Web.Cookie (SetCookie(..), defaultSetCookie, sameSiteStrict, sameSiteLax)
import           Data.Time (secondsToDiffTime, getCurrentTime)
import           Text.Hamlet as H (Html)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Bits (shiftL)
import           MarkdownParsing (purifyMarkdown, text2Html, blockquoteMarkdown)
import           Data.Time.Clock.System (systemSeconds, getSystemTime)
import           Data.Binary (decode, encode)
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Crypto.Hash.SHA256 as SHA256 (hash)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Web.ClientSession as C (Key(..), encryptIO, decrypt)
import           Data.Binary as B (Word64, get, put, Word8, Binary(..))
import           Data.ByteString as BS (length, unpack, ByteString)
import           TextParsing (isAllowed)
import           Network.Wai.Internal (Request(..))
import           Network.Socket (SockAddr(..))

import           Text.Read (readMaybe)
import           Auth (checkAuth, dropError, successMsg, grantAuth)
import           RateLimiting (rateLimiting)
import		     MainConfig
import           Cookies
import           Captcha (finalSize)
import           TextValidate
import           Static (resultsPerThreadPage)

encryptIp :: C.Key -> SockAddr -> IO Text
encryptIp key (SockAddrInet port host) = do
    encrypted <- encryptIO key (toStrict $ encode host)
    return $ decodeUtf8 encrypted
encryptIp key _ = encryptIp key (SockAddrInet 443 0)

threadTimestamp :: Int64 -> Int -> Word64
threadTimestamp currenttime threadid =
    (shiftL (fromIntegral currenttime :: Word64) 32 :: Word64) + (fromIntegral threadid :: Word64)

postId :: Int -> Text
postId v = pack $ "p" ++ show v

------------------------------
-- Handling Message posting --
------------------------------

maxMsgSize = 4096

postProtection :: Handler Html -> Handler Html
postProtection f = do
    checkCsrfParamNamed defaultCsrfParamName

    xpos <- lookupSessionBS "xpos"
    ypos <- lookupSessionBS "ypos"

    xuser <- lookupPostParam "img.x"
    yuser <- lookupPostParam "img.y"

    hasAuth <- checkAuth "captcha"

    if not hasAuth then do

        dropError "error" "authorization expired"

    else do

        grantAuth "captcha"

        case authorized xpos ypos xuser yuser of
            Just True -> do f
            _ -> do dropError "error" "captcha verification error"

    where 

        authorized :: Maybe ByteString -> Maybe ByteString -> Maybe Text -> Maybe Text -> Maybe Bool
        authorized xpos ypos xuser yuser = do
            x <- Data.Binary.decode . fromStrict <$> xpos
            y <- Data.Binary.decode . fromStrict <$> ypos
            xunpacked <- xuser
            yunpacked <- yuser
            xtrue <- readMaybe $ Data.Text.unpack xunpacked 
            ytrue <- readMaybe $ Data.Text.unpack yunpacked
            return $ and [xtrue > x, 
                          xtrue < x + fst finalSize, 
                          ytrue < y + snd finalSize, 
                          ytrue > y]

postPostR :: Int -> Handler H.Html
postPostR v = rateLimiting 5 >> postProtection (handlePost v)

handlePost :: Int -> Handler Html
handlePost v = do
    MathJobsApp {..} <- getYesod

    thread  <- runDB $ getBy (UniqueThreadId v)
    render <- getUrlRender
    request <- waiRequest

    encrypted <- liftIO $ encryptIp randomKey (remoteHost request)

    case thread of
        Nothing -> do notFound
        Just (Entity id threadData) -> do

            let threadid = threadThreadId threadData
            let numPost = threadNumPost threadData
            let curPostId = threadCurPostId threadData
            let numPost = threadNumPost threadData

            result <- lookupPostParam "msg"

            let validate = validatorSettings allowedList domainsList
            let msgsettings = validate { description = "message", cutoff = maxMsgSize }

            case checkValid msgsettings (purifyMarkdown <$> result) of

                Right (text, msg) -> do
                    val <- postCookieR v
                    currenttime <- liftIO $ systemSeconds <$> getSystemTime
                    
                    runDB $ update id [ThreadNumPost +=. 1]
                    runDB $ update id [ThreadCurPostId +=. 1]
                    runDB $ update id [ThreadLastPostTime =. currenttime]
                    runDB $ update id [ThreadTimestamp =. ThreadForms.threadTimestamp currenttime threadid]

                    runDB $ insert_ $ Post {
                        postThreadId = v,
                        postPostId = curPostId + 1,
                        postAuthorCookieId = val,
                        postContent = text,
                        postUpVotes = 0,
                        postDownVotes = 0,
                        postUpVotedBy = [],
                        postDownVotedBy = [],
                        postReported = False,
                        postReportedBy = [],
                        postMarkdownContent = msg,
                        postPostTime = currenttime 
                    }
                    
                    runDB $ insert_ $ IpLogEncrypted {
                        ipLogEncryptedThreadId = v,
                        ipLogEncryptedPostId = curPostId + 1,
                        ipLogEncryptedIp = encrypted,
                        ipLogEncryptedPostTime = currenttime
                    }

                    successMsg "ok" $ returnUrl render v numPost curPostId 

                Left err -> do
                    dropError "error" err

    where

        getLastPage v = case v `mod` resultsPerThreadPage of
            0 -> div v resultsPerThreadPage
            _ -> 1 + div v resultsPerThreadPage

        returnUrl render v threadNumPost curPostId = 
            Data.Text.concat [render (ThreadR v (getLastPage threadNumPost)), 
                              pack "#", 
                              postId (curPostId + 1) ]



