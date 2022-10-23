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


{-# LANGUAGE RecordWildCards			#-}
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
{-# LANGUAGE DataKinds 			    	#-}
{-# LANGUAGE FlexibleInstances 		    	#-}

module Static where

import Yesod
import Data.ByteString
import Yesod.Core.Handler
import MainConfig
import Data.FileEmbed (embedFile)
import NavigationBar (shortBar)
import Data.Text (pack)

-- | Those need to be universal variables

resultsPerThreadPage :: Int
resultsPerThreadPage = 20

-- | Number of threads listed on the frontpage

resultsPerPage :: Int
resultsPerPage = 40

lastPage thread = getLastPage (threadNumPost thread) 

getLastPage v = case v `mod` resultsPerThreadPage of
    0 -> div v resultsPerThreadPage
    _ -> 1 + div v resultsPerThreadPage

altClass i | mod i 2 == 0 = toHtml $ Data.Text.pack "alt"
altClass i | mod i 2 == 1 = toHtml $ Data.Text.pack ""

-- TODO: Next function doesn't belong here.

-- | Generate the default <a href=""> with a navigation bar
-- for thread with index v

defaultLink thread =
    let
        v = threadThreadId thread
        threadtitle = threadThreadTitle thread
    in  [whamlet|
        <a href=@{ThreadR v 1}>
            #{threadtitle} 
        ^{defaultImages' v (lastPage thread)}
        |]
    where
        defaultImages' threadid lastpage
            | lastpage <= 1 = shortBar [[]] Nothing (ThreadR threadid)
            | lastpage <= 5 = shortBar [[1..lastpage]] Nothing (ThreadR threadid)
            | otherwise = shortBar [[1,2],[lastpage - 2, lastpage - 1, lastpage]] Nothing (ThreadR threadid)


robots :: ByteString
robots = $(embedFile "static/robots.txt")

getRobotsR :: Handler Html
getRobotsR = do
    sendResponse $ TypedContent typePlain $ toContent robots

favicon :: ByteString
favicon = $(embedFile "static/favicon.ico")

getIconR :: Handler Html
getIconR = do
    replaceOrAddHeader "Cache-Control" "max-age=604800"
    sendResponse $ TypedContent "image/x-icon" $ toContent favicon

standardStylesheet :: WidgetFor MathJobsApp ()
standardStylesheet = do
    addStylesheet $ StaticR style_css
    toWidgetHead [lucius|               
        a.page-numbers, span.page-numbers {
            border-right:1px solid #bbb;
            border-bottom:1px solid #bbb;
            margin-left: 5px;
            background-image:url(@{StaticR background_gif});
            background-repeat:no-repeat;
            background-position:0 0;
            padding:4px 4px 2px 5px 
        }|]
