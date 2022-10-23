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
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies		#-}
{-# LANGUAGE UndecidableInstances 	#-}
{-# LANGUAGE DataKinds 			#-}
{-# LANGUAGE FlexibleInstances 		#-}

module Cookies where

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
import           Data.Time.Clock.System (systemSeconds, getSystemTime)
import           Data.Binary (decode, encode)
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Crypto.Hash.SHA256 as SHA256 (hash)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Web.ClientSession as C (Key(..), encryptIO, decrypt)
import           Data.Binary as B (Word64, get, put, Word8, Binary(..))
import           Data.ByteString as BS (length, unpack, ByteString)
import           TextParsing (isAllowed)
import           MarkdownParsing (purifyMarkdown, text2Html, blockquoteMarkdown)
import           Network.Wai.Internal (Request(..))
import           Network.Socket (SockAddr(..))
import		     MainConfig


sha256sumAscii :: Text -> Text
sha256sumAscii v = Data.Text.take 16 $ toAscii $ SHA256.hash $ encodeUtf8 v

toAscii :: BS.ByteString -> Text
toAscii bs =
    let w = BS.unpack bs
    in      Data.Text.concat $ Data.Text.singleton . word8toLetter <$> w

word8toLetter :: Word8 -> Char
word8toLetter w = toEnum $ 97 + ((fromIntegral w) `mod` 26)

-- | Gets the cookie ID value. 

getCookieVal :: HandlerFor MathJobsApp Text
getCookieVal = do
    sessioncookie <- getSessionFullCookieVal
    persistentcookie <- getPersistentFullCookieVal
    case (sessioncookie, persistentcookie) of
        (_, Just (CookieData t1 t2)) -> do
            setSessionFullCookieVal (CookieData t1 t2)
            return t1
        (CookieData t1 t2, Nothing) -> do
            return t1

-- | Set the value of the session cookie to corresponding CookieData

setSessionFullCookieVal :: CookieData -> HandlerFor MathJobsApp ()
setSessionFullCookieVal v = do
    setSessionBS "cookieData" $ toStrict $ encode v

generateValue = do
    rand <- liftIO $ stringRandomIO $ pack "[a-z]{16}"
    return rand 

-- | Get the session cookie CookieData
-- This always return a value; if session cookie data
-- is not available it creates it

getSessionFullCookieVal :: HandlerFor MathJobsApp CookieData
getSessionFullCookieVal = do
    cookie <- lookupSessionBS "cookieData"
    case (decode . fromStrict <$> cookie) :: Maybe CookieData of
        Just (CookieData t1 t2) -> do
            return $ CookieData t1 t2
        _ -> do
            rand <- generateValue
            setSessionFullCookieVal (CookieData rand True)
            return (CookieData rand True)

-- | Encodes the value of the cookie. 
-- The second value determines if it's additionally hashed

data CookieData = CookieData Text Bool
    deriving (Show)

-- | Allows to serialize/deserialize the CookieData

instance Binary CookieData where
    put (CookieData t1 t2) = do
        B.put t1
        B.put t2

    get = do
        t1 <- B.get
        CookieData t1 <$> B.get

-- | Updates the value of the persistent cookie
-- by copying the session cookie into the persistent
-- cookie

updatePersistentCookie :: HandlerFor MathJobsApp ()
updatePersistentCookie = do
    sessioncookie <- getSessionFullCookieVal
    setPersistentFullCookieVal sessioncookie

-- | Update the session cookie so that it uses 
-- For this to work importantly upon state change
-- we need to regenerate the value

makeSessionPerThread :: Bool -> HandlerFor MathJobsApp Text
makeSessionPerThread False = do
    sessioncookie <- getSessionFullCookieVal
    case sessioncookie of
        CookieData t1 True -> do return t1
        CookieData t1 False -> do
            setSessionFullCookieVal (CookieData t1 True)
            return t1

makeSessionPerThread True = do
    rand <- generateValue 
    setSessionFullCookieVal (CookieData rand True)
    return rand

clearSessionPerThread :: Bool -> HandlerFor MathJobsApp Text
clearSessionPerThread False = do
    CookieData t1 _ <- getSessionFullCookieVal
    setSessionFullCookieVal (CookieData t1 False)
    return t1

clearSessionPerThread True = do
    rand <- generateValue 
    setSessionFullCookieVal (CookieData rand False)
    return rand

isPersistent :: HandlerFor MathJobsApp Bool
isPersistent = do
    cookie <- getPersistentFullCookieVal
    case cookie of
        Just _ -> do return True
        Nothing -> do return False

isPerThread :: HandlerFor MathJobsApp Bool
isPerThread = do
    CookieData _ t <- getSessionFullCookieVal
    return t

deletePersistent :: HandlerFor MathJobsApp ()
deletePersistent = do
    persistent <- isPersistent
    if persistent then do
        expirePersistentCookie
    else do return ()

setPersistentFullCookieVal :: CookieData -> HandlerFor MathJobsApp ()
setPersistentFullCookieVal val = do
    MathJobsApp {..} <- getYesod
    value <- liftIO $ C.encryptIO key $ Data.ByteString.Lazy.toStrict (Data.Binary.encode val)
    setCookie (cookie value)

    where
        cookie :: ByteString -> SetCookie
        cookie val = defaultSetCookie {
                setCookieName = encodeUtf8 $ pack "persistent",
                setCookieValue = val,
                setCookieMaxAge = Just $ secondsToDiffTime (60*60*24*365*10),
                setCookieDomain = Nothing, -- Just $ encodeUtf8 $ pack "mathjobrumors.com"
                setCookieHttpOnly = True,
                setCookieSecure = True,
                setCookieSameSite = Just sameSiteStrict,
                setCookiePath = Just $ encodeUtf8 $ pack "/"
        }

expirePersistentCookie :: HandlerFor MathJobsApp ()
expirePersistentCookie = do
    MathJobsApp {..} <- getYesod
    setCookie cookie

    where

        cookie :: SetCookie
        cookie = defaultSetCookie {
                setCookieName = encodeUtf8 $ pack "persistent",
                setCookieValue = encodeUtf8 $ pack "",
                setCookieMaxAge = Nothing, -- Just $ secondsToDiffTime (60*60*24*365*10),
                setCookieDomain = Nothing, -- Just $ encodeUtf8 $ pack "mathjobrumors.com", 
                setCookieHttpOnly = True,
                setCookieSecure = True,
                setCookieSameSite = Just sameSiteLax,
                setCookiePath = Just $ encodeUtf8 $ pack "/"
        }

getPersistentFullCookieVal :: HandlerFor MathJobsApp (Maybe CookieData)
getPersistentFullCookieVal = do
    MathJobsApp {..} <- getYesod
    cookie <- lookupCookie "persistent"
    case cookie of
        Just val -> do
            return (decode <$> fromStrict <$> C.decrypt key (encodeUtf8 val) :: Maybe CookieData)
        Nothing -> do return Nothing

getFullUserName :: Int -> HandlerFor MathJobsApp Text
getFullUserName v = do
    sessioncookie <- getSessionFullCookieVal
    case sessioncookie of
        CookieData t1 True -> do
            return $ sha256sumAscii $ Data.Text.concat [t1, pack $ show v]
        CookieData t1 False -> do
            return t1

-- | updates the cookie and returns the new username

changeState perthreadcookie persistentcookie = do
    persistent <- isPersistent
    perthread <- isPerThread
    let perthreadcookieresult = perthreadcookie == Just "True"
    let persistentcookieresult = persistentcookie == Just "True"
    if (perthreadcookieresult /= perthread) || (persistentcookieresult /= persistent) then
        do return True
    else
        do return False

postCookieR :: Int -> HandlerFor MathJobsApp Text
postCookieR v = do
    perthread <- lookupPostParam "perthread"
    persistent <- lookupPostParam "persistent"
    changestate <- changeState perthread persistent
    case (perthread, persistent) of
        (Just "True", Just "True") -> do
            newid <- makeSessionPerThread changestate
            updatePersistentCookie
            return $ sha256sumAscii $ Data.Text.concat [newid, pack $ show v]
        (Just "True", Nothing) -> do
            newid <- makeSessionPerThread changestate
            deletePersistent
            return $ sha256sumAscii $ Data.Text.concat [newid, pack $ show v]
        (Nothing, Just "True") -> do
            newid <- clearSessionPerThread changestate
            updatePersistentCookie
            return newid
        (Nothing, Nothing) -> do
            newid <- clearSessionPerThread changestate
            deletePersistent
            return newid

