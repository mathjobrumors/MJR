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

module RateLimitIP where

import Data.Int (Int64)
import Yesod
import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Text
import Auth
import qualified Data.Map as M
import Network.Wai 
import Data.Time.Clock.System
import Network.HTTP.Types.Status
import Captcha (sha256sum)

newtype RateLimit = RateLimit (M.Map Text Integer) deriving Show

updateRateLimit :: TVar (M.Map Int64 RateLimit) -> Integer -> Int64 -> Text -> IO Bool
updateRateLimit tvar cutoff delay key = do 
   
    time <- systemSeconds <$> getSystemTime

    -- Only keep unexpired entries
    atomically $ modifyTVar tvar (M.filterWithKey (\k _ -> k >= time))

    allowed <- atomically $ stateTVar tvar (\a -> (isAllowed key cutoff (snd <$> (M.toList a)), a))

    if allowed then do
        atomically $ modifyTVar tvar (updateRate time delay key)
        return True
    else do
        return False

    where

    isAllowed :: Text -> Integer -> [RateLimit] -> Bool       
    isAllowed key cutoff (RateLimit x:xs) = case M.lookup key x of
        
        Just value -> 
            if   value >= cutoff then False 
            else isAllowed key cutoff xs

        Nothing    -> isAllowed key cutoff xs
    
    isAllowed key cutoff [] = True 

    updateRate :: Int64 -> Int64 -> Text -> M.Map Int64 RateLimit -> M.Map Int64 RateLimit
    updateRate time delay key rate = 
        M.insertWith (\(RateLimit new) (RateLimit old) -> RateLimit $ M.insertWith (+) key 1 old) 
            (time + delay) (RateLimit $ M.singleton key 1) rate 

-- 20 requests per 10 second rate limiting

rateLimitApplication :: Text -> TVar (M.Map Int64 RateLimit) -> Application -> Application 
rateLimitApplication nonce tvar f r k = do 
    isAllowed <- updateRateLimit tvar 20 10 (requestToId r nonce)
    if isAllowed then do
        f r k
    else do
        k $ responseLBS status200 [] "Rate Limiting"

requestToId :: Request -> Text -> Text
requestToId req nonce = sha256sum $ Data.Text.concat 
    [pack $ show $ Auth.getIpInt $ Just $ remoteHost req, 
     nonce] 