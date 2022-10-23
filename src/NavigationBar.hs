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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE DerivingStrategies		#-}
{-# LANGUAGE StandaloneDeriving 	#-}
{-# LANGUAGE UndecidableInstances 	#-}
{-# LANGUAGE DataKinds 		    	#-}
{-# LANGUAGE FlexibleInstances 		#-}
{-# LANGUAGE BlockArguments         	#-}

module NavigationBar where

import           Data.Text               (Text, pack, take, concat, intercalate, replace)
import           Yesod
import           Yesod.Core
import           Data.Time (secondsToDiffTime)
import           MainConfig

-- | Generate the code for the navigation bar at the
-- bottom of the listing of threads. Takes as argument
-- the number of threads (numpages), the cutoff at which
-- we start to compress the listing, a routing function
-- that assigns to integer indices the corresponding URL
-- of the ith page, and finally the current page.  

navigationBar :: Int -> Int -> (Int -> Route MathJobsApp) -> Int -> WidgetFor MathJobsApp ()
navigationBar numpages cutoff route page = 
        shortBar (controller numpages cutoff page) (Just page) route 

    where 

        -- Describes the logic of how we cutoff the navigation bar

        controller :: Int -> Int -> Int -> [[Int]]
        controller numpages cutoff page 
            | numpages > cutoff = case (page, numpages - page) of
                (1,_) -> [[1,2],[numpages]]
                (2,_) -> [[1,2,3],[numpages]]
                (3,_) -> [[1,2,3,4],[numpages]]
                (_,2) -> [[1],[numpages - 3, numpages - 2, numpages - 1, numpages]]
                (_,1) -> [[1],[numpages - 2, numpages - 1, numpages]]
                (_,0) -> [[1],[numpages - 1, numpages]]
                _ -> [[1], [page - 1, page, page + 1], [numpages]]
            | numpages > 1 = [[1..numpages]]
            | otherwise = [[]] 

-- | Generates a bar of pages
-- The [[Int]] is used to specify the location of ... 
-- The Maybe Int specifies the current page
-- The routing function assigns routing to indices

shortBar :: [[Int]] -> Maybe Int -> (Int -> Route MathJobsApp) -> WidgetFor MathJobsApp ()
shortBar list cur route = 
    let tuples = zip list [1..]
    in 
    [whamlet|
        $forall l <- tuples
            ^{shorterBar (fst l) cur route}
            $if (/=) (snd l) (length list)
                <span class="page-numbers dots">
                    ...
    |]

    where
                
        shorterBar :: [Int] -> Maybe Int -> (Int -> Route MathJobsApp) -> WidgetFor MathJobsApp ()
        shorterBar list cur route = 
            [whamlet|
                $forall i <- list
                    $if Just i == cur    
                        <span class="page-numbers current"><a href=@{route i}>#{i}</a>
                    $else
                        <a href=@{route i} class="page-numbers">#{i}
            |]
