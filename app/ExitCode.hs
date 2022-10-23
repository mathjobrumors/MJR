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

module ExitCode where

import Data.Set
import Database.Persist
import Auth
import MainConfig
import Database.Persist.Sqlite
import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import System.IO
import Yesod

saveTokens pool shared = do
    
    flip runSqlPersistMPool pool $ do 

        -- Save new authorizations

        tvar <- liftIO $ atomically $ readTVar shared  
        sequence_ $ fmap commit $ Data.Set.toList tvar 

        -- Delete old authorizations

        oldauth <- selectList [] [] 
        let oldauth' = fmap (\(Entity id content) -> Authorization 
                        (authorizationDBAuthId content)
                        (authorizationDBAuthTime content)) oldauth
        
        sequence_ $ fmap deleteBy 
                  $ fmap (\(Authorization id time) -> UniqueAuthId id) 
                  $ Data.Set.toList
                  $ Data.Set.difference (Data.Set.fromList oldauth') tvar

        return ()

    print "tokens saved"

    where

        commit (Authorization id time) = do
            val <- getBy $ UniqueAuthId id
            case val of
                Just (Entity id content) -> do return ()
                Nothing             -> do 
                    insert_ (AuthorizationDB id time)
                    return ()

-- | Purge encrypted IPs

purgeIps pool = do
    
    flip runSqlPersistMPool pool $ do 

        iplist <- selectList [] []
        sequence_ $ 
            fmap deleteBy $ 
            fmap (\(Entity id content) -> 
                    UniqueThreadPostIp 
                        (ipLogEncryptedThreadId content) (ipLogEncryptedPostId content)) iplist 
