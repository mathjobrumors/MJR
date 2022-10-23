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

module SetupDB where 

import Database.Persist.Sqlite
import Data.Set
import Data.Text
import MainConfig
import Yesod
import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

getSticky (x:xs) = do
    thread <- getBy $ UniqueThreadId x
    case thread of
        Just (Entity id content) -> do
            newlist <- getSticky xs
            return (Entity id (content {threadThreadTitle = Data.Text.concat [ pack "\128204 ", threadThreadTitle content ]}):newlist)
        Nothing -> do
            getSticky xs
getSticky [] = do return []

makeSticky list val = 
    sequence_ $ fmap (\(Entity id _) -> update id [ThreadSticky =. val]) list

purgeTVar shared cutoff = do
    size <- stateTVar shared (\s -> (size s, s)) 
    if size > cutoff then
        modifyTVar' shared (\x -> Data.Set.deleteMin x)
    else
        return ()

-- | Purges old authorizations and 
-- sets the sticky tag. Load authorization into shared
-- Returns list of sticky threads. 

-- TODO: Purge likes and dislikes from old inactive threads?

setupDB stickylist timenow pool shared = flip runSqlPersistMPool pool $ do

    runMigration migrateAll

    -- Purge old Authorizations (older than a week)

    oldauth <- selectList [AuthorizationDBAuthTime <=. timenow - (60*60*24*7)] []       
    sequence_ $ 
        fmap deleteBy $ 
        fmap (\(Entity id content) -> UniqueAuthId $ authorizationDBAuthId content) oldauth 

    currentauth <- selectList [] [] 
    liftIO $ atomically $ writeTVar shared $ Data.Set.fromList $ 
        fmap (\(Entity id content) -> Authorization (authorizationDBAuthId content) (authorizationDBAuthTime content)) currentauth

    -- Sticky initialization

    list <- getSticky $ read <$> Prelude.lines stickylist 
    stickynow <- selectList [ThreadSticky ==. True] []
    makeSticky stickynow False
    makeSticky list True

    liftIO $ putStrLn "Database initialized"

    liftIO $ atomically $ purgeTVar shared $ 128*1024

    return list
