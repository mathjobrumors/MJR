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

module PostTopic where

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
import Cookies (isPerThread, isPersistent, postCookieR)
import Captcha (captchaFrame)
import           Data.Set as Set (member, Set(..), unions, fromList, toList)
import Static (standardStylesheet, defaultLink, resultsPerPage, resultsPerThreadPage)
import TimeFormat
import Tree
import TextValidate
import Text.Blaze.Internal

postTopicR :: Handler Html
postTopicR =
    rateLimiting 5 >> postProtection topicSubmit

-- We check csrf and updateTimestamp in rateLimiting
-- no need to do this here again

topicSubmit :: Handler Html
topicSubmit = do

    title <- lookupPostParam "topic"
    msg   <- lookupPostParam "msg"

    MathJobsApp {..} <- getYesod
    request <- waiRequest
    encrypted <- liftIO $ encryptIp randomKey (remoteHost request)

    currentTime <- liftIO $ systemSeconds <$> getSystemTime

    -- TODO: Check message cutoff

    let settings = validatorSettings allowedList domainsList
    let titlesettings = settings { description = "title",   cutoff = 80 }
    let msgsettings   = settings { description = "message", cutoff = 4096 }

    maxThreadId <- runDB $ getBy $ UniqueIdent 1

    case (checkValid titlesettings title, 
          checkValid msgsettings   msg,
          maxThreadId) of

        (Right (ftitle,otitle), 
         Right (fmsg, omsg), 
         Just  (Entity id maxthread)) -> do
    
            let newThreadId = maxIdThread maxthread + 1
            let numThreads = maxIdNumThread maxthread + 1;

            val <- postCookieR newThreadId 
            
            runDB $ update id [ MaxIdThread =. newThreadId ]
            runDB $ update id [ MaxIdNumThread =. numThreads ]

            runDB $ insert_ $ Thread {
                threadThreadId = newThreadId,
                threadThreadTitle = otitle, 
                threadLastPostTime = currentTime,
                threadNumPost = 1, 
                threadNumViews = 1, 
                threadNumUpVotes = 0, 
                threadNumDownVotes = 0, 
                threadTimestamp = ThreadForms.threadTimestamp currentTime newThreadId, 
                threadCurPostId = 1, 
                threadSticky = False
            } 
            
            runDB $ insert_ $ Post {
                postThreadId = newThreadId, 
                postPostId = 1, 
                postAuthorCookieId = val, 
                postContent = fmsg, 
                postUpVotes = 0, 
                postDownVotes = 0, 
                postUpVotedBy = [], 
                postDownVotedBy = [],
                postReported = False, 
                postReportedBy = [], 
                postMarkdownContent = omsg, 
                postPostTime = currentTime
            }

            runDB $ insert_ $ IpLogEncrypted {
                ipLogEncryptedThreadId = newThreadId,
                ipLogEncryptedPostId = 1, 
                ipLogEncryptedIp = encrypted, 
                ipLogEncryptedPostTime = currentTime
            }
        
            successMsg "ok" "submitted"

        (Left err, _, _) -> do dropError "error" err
        (_, Left err, _) -> do dropError "error" err
        (_, _, Nothing)  -> do dropError "error" "catastrophic database failure"