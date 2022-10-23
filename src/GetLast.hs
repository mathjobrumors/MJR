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

module GetLast where

import Yesod
import Thread (getThreadR)
import Cookies (getFullUserName)
import RateLimiting (updateTimestamp)    
import MainConfig
import Text.Hamlet
import Data.Text  as T (concat, pack)
import Static 

------------------------------------
--- Thread page HTML source code ---
------------------------------------

getThreadLastUserR :: Int -> Handler Text.Hamlet.Html
getThreadLastUserR v = do
    updateTimestamp
    
    val <- getFullUserName v
    
    render <- getUrlRender
    
    thread <- runDB $ getBy (UniqueThreadId v)
    
    mostrecent <- runDB $ selectList 
            [PostThreadId ==. v, PostAuthorCookieId ==. val] 
            [Desc PostPostId, LimitTo 1]
    
    case mostrecent of

        [] -> do invalidArgs ["no posts by user"] 
    
        [Entity id found] -> do
            case thread of
                Just (Entity id thread) ->
                       redirect $ T.concat [render (ThreadR v (getLastPage (threadNumPost thread))),
                                pack "#p",
                                pack (show $ postPostId found) ]

                _ -> invalidArgs ["Database error"]
                
        _ -> invalidArgs ["Database error"]

-- | GET Handler
-- Output the last page of thread v

getThreadLastR :: Int -> Handler Html
getThreadLastR v = do
    updateTimestamp
    thread <- runDB $ getBy (UniqueThreadId v)
    case thread of
        Just (Entity _ t) -> do
            getThreadR v (lastPage t)
        _ -> do notFound
