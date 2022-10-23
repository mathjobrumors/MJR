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
{-# LANGUAGE DataKinds 		        	#-}
{-# LANGUAGE FlexibleInstances 		    #-}
{-# LANGUAGE BangPatterns               #-}


module MainConfig where

import           Data.Text (Text, pack, take, concat, intercalate)
import           Database.Persist.Sqlite
import           Yesod
import           Yesod.Core
import           Data.Int
import           Text.StringRandom (stringRandomIO)
import           Web.Cookie (SetCookie(..))
import           Data.Time (secondsToDiffTime)
import           Data.Word
import           Yesod.EmbeddedStatic (embedFileAt, concatFiles, mkEmbeddedStatic, EmbeddedStatic(..)) 
import           Data.ByteString  
import           Web.ClientSession as C (Key(..))
import           Network.Socket (SockAddr(..))
import           Network.Wai.Internal (Request(..))
import           Tree
import           Data.Set as Set (member, Set(..), unions, fromList, toList)
import           Data.IORef
import           Data.Binary as B 
import           Control.Monad.State.Lazy
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

-- This properly belongs in Auth.hs 

data Authorization = Authorization {
    id :: Text, 
    timestamp :: Int64
} deriving (Show)

instance Binary Authorization where
    put a = do
        B.put $ MainConfig.id a
        B.put $ MainConfig.timestamp a

    get = do
        t1 <- B.get
        t2 <- B.get
        return $ Authorization { MainConfig.id = t1, 
                                 MainConfig.timestamp = t2 }

instance Eq Authorization where
    (==) a b = and [MainConfig.timestamp a == MainConfig.timestamp b, 
                    MainConfig.id a == MainConfig.id b]

instance Ord Authorization where
    (>) a b = MainConfig.timestamp a > MainConfig.timestamp b
    (<=) a b = or [MainConfig.timestamp a < MainConfig.timestamp b,
                   and [MainConfig.timestamp a == MainConfig.timestamp b, 
                        MainConfig.id a < MainConfig.id b],
                   a == b]

instance PersistFieldSql SockAddr where
    sqlType _ = SqlInt32

instance PersistField SockAddr where
    toPersistValue (SockAddrInet port host) = PersistInt64 (fromIntegral host :: Int64)
    toPersistValue _ = PersistInt64 0 
    fromPersistValue (PersistInt64 v) = Right $ SockAddrInet 443 (fromIntegral v :: Word32)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

IpLogEncrypted
    UniqueThreadPostIp threadId postId
    threadId Int
    postId Int
    ip Text
    postTime Int64

BannedIp
    UniqueIp ip
    ip SockAddr
    msg Text

MaxId
    UniqueIdent ident 
    ident Int default=1
    thread Int default=1
    numThread Int default=1

Thread
    UniqueThreadId threadId
    threadId Int
    threadTitle Text
    lastPostTime Int64
    numPost Int
    numViews Int
    numUpVotes Int
    numDownVotes Int
    timestamp Word64
    Primary timestamp
    curPostId Int
    sticky Bool default=False

SpamThread
    UniqueSpamThreadId threadId
    threadId Int
    threadTitle Text
    lastPostTime Int64
    numPost Int
    numViews Int
    numUpVotes Int
    numDownVotes Int
    timestamp Word64
    Primary timestamp
    curPostId Int

Post
    UniqueThreadPost threadId postId
    threadId Int
    postId Int
    authorCookieId Text
    content Html
    upVotes Int
    downVotes Int
    upVotedBy [Text]
    downVotedBy [Text]
    reported Bool
    reportedBy [Text]
    markdownContent Text
    postTime Int64

SpamPost
    UniqueSpamThreadPost threadId postId
    threadId Int
    postId Int
    authorCookieId Text
    content Html
    upVotes Int
    downVotes Int
    upVotedBy [Text]
    downVotedBy [Text]
    reported Bool
    reportedBy [Text]
    markdownContent Text
    postTime Int64

CaptchaDB
    UniqueCaptchaID captchaId
    captchaId Int
    captchaImg ByteString
    captchaPosX Int
    captchaPosY Int

AuthorizationDB
    UniqueAuthId authId
    authId Text
    authTime Int64

|]

mkEmbeddedStatic False "staticdir" [embedFileAt "style.css" "static/style.css", 
                                    concatFiles "post.js" ["static/vote.js","static/report.js","static/casework.js"],
                                    concatFiles "home.js" ["static/postmain.js"],
                                    embedFileAt "background.gif" "static/background.gif",
                                    embedFileAt "favicon.ico" "static/favicon.ico"]

data MathJobsApp = MathJobsApp
    { persistConfig     :: SqliteConf
    , connPool          :: ConnectionPool
    , getStatic         :: EmbeddedStatic
    , authorizedTokens  :: [Text]
    , key               :: C.Key
    , domainsList       :: Tree Text
    , allowedList       :: Set Text 
    , stickyList        :: [Entity Thread]
    , randomKey         :: C.Key
    , shared            :: TVar (Set Authorization)
    , randomNonce       :: Text
    , activeConnections :: TVar (Int)
    }

--Alternatively:
--instance YesodPersist MathJobsApp where
--    type YesodPersistBackend MathJobsApp = SqlBackend
--    runDB = defaultRunDB persistConfig connPool

instance YesodPersist MathJobsApp where
    type YesodPersistBackend MathJobsApp = SqlBackend
    
    runDB action = do
        MathJobsApp {..} <- getYesod
        runSqlPool action connPool

mkYesodData "MathJobsApp" $(parseRoutesFile "routes.yesodroutes")

sessionLifetime :: Int
sessionLifetime = 10080 -- a week in minutes

instance Yesod MathJobsApp where

    yesodMiddleware handler = do
        addHeader "Cache-Control" "max-age=0"
        blockIps $ defaultYesodMiddleware $ handler

        where 
            
            blockIps :: HandlerFor MathJobsApp res -> HandlerFor MathJobsApp res
            blockIps handler = do
                req <- waiRequest
                ban <- runDB $ getBy $ UniqueIp (remoteHost req)
                case ban of 
                    Just (Entity _ content) -> do
                        sendResponse (bannedIpMsg content) >> handler
                    _ -> do handler

    makeSessionBackend _ =
            sslOnlySessions . strictSameSiteSessions $
            Just
                <$> customizeSessionCookies addDomain
                <$> defaultClientSessionBackend sessionLifetime "client_session_key.aes"
        where
            addDomain cookie = cookie { setCookieSecure = True, setCookieMaxAge = Nothing, setCookieExpires = Nothing }