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

{-# LANGUAGE FlexibleContexts			  #-}
{-# LANGUAGE GADTs				          #-}
{-# LANGUAGE MultiParamTypeClasses	#-}
{-# LANGUAGE OverloadedStrings			#-}
{-# LANGUAGE TypeFamilies			      #-}
{-# LANGUAGE DerivingStrategies			#-}
{-# LANGUAGE UndecidableInstances		#-}
{-# LANGUAGE DataKinds 			        #-}
{-# LANGUAGE FlexibleInstances    	#-}

module RateLimiting where

import           Data.Text (Text, pack, take, concat, intercalate, unpack)
import           Yesod
import           Data.Int (Int64)
import           Data.Time (secondsToDiffTime)
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.Time.Clock.System (systemSeconds, getSystemTime)
import           MainConfig
import           Data.Binary (encode, decodeOrFail)
import           Auth (dropError, grantAuth, getAuthToken, checkAuth)

updateTimestamp :: Handler Int64
updateTimestamp = do
  getAuthToken "timestamp" -- If the token doesn't exist generate it
  time <- liftIO $ systemSeconds <$> getSystemTime
  setSessionBS "timestamp" (toStrict $ encode time)
  return time

getTimestamp :: Handler Int64
getTimestamp = do
  bs <- lookupSessionBS "timestamp"
  case bs of
    Just t -> do
      case decodeOrFail $ fromStrict t of
        Right (_,_,time) -> do return time
        Left _ -> invalidArgs ["corrupted session cookie"]
    Nothing -> do
          grantAuth "timestamp"
          updateTimestamp

rateLimiting :: Int -> Handler Html
rateLimiting n = do
  isAllowed <- checkAuth "timestamp"
  
  time <- getTimestamp
  curtime <- liftIO $ systemSeconds <$> getSystemTime
  let expectedTime = time + (fromIntegral n :: Int64)

  case (isAllowed, curtime >= expectedTime) of
    (True, False) -> do
      let diff = expectedTime - curtime
      wait diff

    (True, True) -> do 
      grantAuth "timestamp"
      updateTimestamp 
      defaultLayout ""

    (False, _) -> do
      grantAuth "timestamp"
      updateTimestamp
      wait (fromIntegral n :: Int64)
      
    where

      wait :: Int64 -> HandlerFor MathJobsApp Html
      wait n = do 
        dropError "toofast" (Data.Text.concat [pack "wait ",
                            pack $ show n,
                            pack " second",
                            if n > 1 then pack "s" else pack ""])
        defaultLayout "" 
