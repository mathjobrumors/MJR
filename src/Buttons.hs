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
{-# LANGUAGE DataKinds 		    	#-}
{-# LANGUAGE FlexibleInstances 		#-}
{-# LANGUAGE BangPatterns           #-}

module Buttons where

import		     Yesod
import           Data.Text (Text, pack)
import           Database.Persist.Sqlite
import           MarkdownParsing (blockquoteMarkdown)
import           RateLimiting (rateLimiting)
import		     MainConfig
import           Cookies (getFullUserName)

---------------------------------------------------------
-- Handlers for buttons: Upvote, Downvote, Report      --
---------------------------------------------------------

postReportR :: Int -> Int -> Handler Html
postReportR v post = 
    rateLimiting 1 >> reportButton v post

postVUpR :: Int -> Int -> Handler Html
postVUpR v post = 
    rateLimiting 1 >> voteButton logicUpVote v post

    where

        logicUpVote v =
            case v of
                (True, False) -> (False, False)
                (False, False) -> (True, False)
                (False, True) -> (True, False)
                (True, True) -> (False, False)

postVDownR :: Int -> Int -> Handler Html
postVDownR v post = 
    rateLimiting 1 >> voteButton logicDownVote v post

    where
            
        logicDownVote v =
            case v of
                (True, False) -> (False, True)
                (False, False) -> (False, True)
                (False, True) -> (False, False)
                (True, True) -> (False, False)

---------------------------------------
--- Auxiliary functions              --
---------------------------------------


addSet :: (Eq a) => Int -> a -> [a] -> [a]
addSet cutoff val set = 
    if val `elem` set then Prelude.take cutoff set
    else                   Prelude.take cutoff (val:set)

removeSet :: (Eq a) => a -> [a] -> [a]
removeSet val = filter (/= val)

-------------------------------------------------
--- Logic function that brings into the state ---
-------------------------------------------------

reportButton :: Int -> Int -> HandlerFor MathJobsApp Html
reportButton v post = do
    let cutOff = 128 -- We don't allow more than 128 reports
    checkCsrfParamNamed defaultCsrfParamName
    post <- runDB $ getBy (UniqueThreadPost v post)
    case post of
        Nothing -> do invalidArgs ["Database failure"]
        Just (Entity id postcontent) -> do
            val <- getFullUserName v -- getCookieVal
            runDB $ update id [PostReported =. True]
            let reportedby = postReportedBy postcontent
            runDB $ update id [PostReportedBy =. addSet cutOff val reportedby]
            return $ toHtml $ pack ""

voteButton :: ((Bool, Bool) -> (Bool, Bool)) -> Int -> Int -> HandlerFor MathJobsApp Html
voteButton logicTable v post = do
    checkCsrfParamNamed defaultCsrfParamName
    thread <- runDB $ getBy (UniqueThreadId v)
    case thread of
        Nothing -> do invalidArgs ["database failure"]
        Just (Entity threadNumericalId threadcontent) -> do
            post <- runDB $ getBy (UniqueThreadPost v post)
            case post of
                Nothing -> do invalidArgs ["database failure"]
                Just (Entity id postcontent) -> do
                    val <- getFullUserName v
                    let upvotedby = postUpVotedBy postcontent
                    let downvotedby = postDownVotedBy postcontent
                    let truthtable = logicTable (val `elem` upvotedby, val `elem` downvotedby)
                    executeState truthtable upvotedby downvotedby val id threadNumericalId
                    return $ toHtml $ pack "" -- Return no content

executeState :: (Bool, Bool) -> [Text] -> [Text] -> 
        Text -> Key Post -> Key Thread -> HandlerFor MathJobsApp ()

executeState (a,b) upvotedby downvotedby val id threadId = do
    -- We don't allow more than 1024 likes or dislikes. 
    let (newupvotedby, newdownvotedby) = upVoteState 1024 (a,b) upvotedby downvotedby val
    let newupvotes = Prelude.length newupvotedby
    let newdownvotes = Prelude.length newdownvotedby
    let oldupvotes = Prelude.length upvotedby
    let olddownvotes = Prelude.length downvotedby

    runDB $ update id [PostUpVotes =. newupvotes, 
                       PostDownVotes =. newdownvotes,
                       PostDownVotedBy =. newdownvotedby,
                       PostUpVotedBy =. newupvotedby]

    runDB $ update threadId [ThreadNumUpVotes +=. newupvotes - oldupvotes,
                             ThreadNumDownVotes +=. newdownvotes - olddownvotes]

    where

        upVoteState :: Int -> (Bool, Bool) -> [Text] -> [Text] -> Text -> ([Text], [Text])
        upVoteState cutoff v up down val =
            case v of
                (True, False) -> (addSet cutoff val up, removeSet val down)
                (False, True) -> (removeSet val up, addSet cutoff val down)
                (False, False) -> (removeSet val up, removeSet val down)
                (True, True) -> (addSet cutoff val up, addSet cutoff val down)
