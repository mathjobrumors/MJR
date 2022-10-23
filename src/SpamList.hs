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
{-# LANGUAGE DerivingStrategies		#-}
{-# LANGUAGE StandaloneDeriving 	#-}
{-# LANGUAGE UndecidableInstances 	#-}
{-# LANGUAGE DataKinds 			#-}
{-# LANGUAGE FlexibleInstances 		#-}

module SpamList where

import           Data.Text (Text, pack, take, concat, intercalate, replace)
import           Yesod
import           Data.Int (Int64)
import           Data.Time (secondsToDiffTime)
import           MainConfig
import           ThreadForms (postId)
import           Data.Time.Clock.System (systemSeconds, getSystemTime)
import           NavigationBar (navigationBar)
import           Static (standardStylesheet)
import           TimeFormat


resultsPerSpamPage :: Int
resultsPerSpamPage = 40

lastSpamPage numpost = case numpost `mod` resultsPerSpamPage of
    0 -> div numpost resultsPerSpamPage
    _ -> 1 + div numpost resultsPerSpamPage

getSpamListR :: Int -> Handler Html
getSpamListR page
    | page <= 0 = do notFound
    | otherwise = do
        let v = page
        let resultsPerPage = 40
        let res = resultsPerSpamPage

        currentTime <- liftIO $ systemSeconds <$> getSystemTime

        posts <- runDB $ selectList
            [ SpamPostPostTime >=. (currentTime - (60*60*24*7))]
            [ Desc SpamPostPostTime
            , LimitTo resultsPerSpamPage
            , OffsetBy ((page - 1) * resultsPerSpamPage)
            ]

        -- Potentially slow

        allposts <- runDB $ selectList [SpamPostPostTime >=. (currentTime - (60*60*24*7))] [Desc SpamPostPostTime]

        defaultLayout $ do
            let numpost = length allposts
            let res = resultsPerSpamPage

            toWidgetHead [hamlet|<meta name="viewport" content="width=device-width">|]

            standardStylesheet

            addScriptRemote "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"

            setTitle $ toHtml $ pack "Last week's spam"

            let iteratedposts = zip posts [1..]
            toWidget $(whamletFile "spam.html")

        where
        unMaybe :: Maybe Text -> Text
        unMaybe (Just x) = x
        unMaybe Nothing = ""

getSpamTitle :: SpamPost -> HandlerFor MathJobsApp Text
getSpamTitle post = do
    spamthread <- runDB $ getBy (UniqueSpamThreadId $ spamPostThreadId post)
    okthread   <- runDB $ getBy (UniqueThreadId $ spamPostThreadId post)
    case (spamthread, okthread) of
        (Just (Entity _ content), _) -> do
            return $ spamThreadThreadTitle content
        (_, Just (Entity _ content)) -> do
            return $ threadThreadTitle content
        _ -> do
            return $ pack ""

spamRoute :: Int -> Route MathJobsApp
spamRoute = SpamListR

lastPage spams = case spams `mod` resultsPerSpamPage of
    0 -> div spams resultsPerSpamPage
    _ -> 1 + div spams resultsPerSpamPage

postEntrySpam :: SpamPost -> Int64 -> WidgetFor MathJobsApp ()
postEntrySpam post currenttime = do
        title <- handlerToWidget $ getSpamTitle post
        [whamlet|
            <div id=#{postId (spamPostPostId post)}>

                <div class="threadauthor" style="marging-top:0.25em">    
                        <strong>Top Spammer
                        <br>
                        <small>#{toHtml $ Data.Text.take 4 $ spamPostAuthorCookieId post}  

                <div class="threadpost">
                    <div class="post">
                        #{spamPostContent post}
                
                    <div class="poststuff">
                        #{formatTime Long (currenttime - (spamPostPostTime post))}

                        <span class="rb">
                            #{toHtml $ length $ spamPostReportedBy post} Report

                        <br>

                        Title : #{toHtml $ title}



                |]
