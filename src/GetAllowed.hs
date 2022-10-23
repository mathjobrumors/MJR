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

module GetAllowed where

import MainConfig
import Yesod
import Text.Hamlet
import Data.Set (toList)
import Data.Text (unpack, pack, concat, Text(..), intercalate)
import Text.Julius

getAllowedR :: Handler Html
getAllowedR = do
    MathJobsApp {..} <- getYesod
    defaultLayout $ do
        setTitle $ toHtml $ pack "Allowed domains"
        [whamlet|
            <h1>
                Allowed domains
            <ol>
                $forall s <- allowedList
                    <li>
                        #{toHtml s}
        |]

allowedListJS allowedlist = do 
    toWidgetHead [julius|    
        var allowed=[#{listJS}]
    |]

    where

        listJS = rawJS $ 
            Data.Text.intercalate "," $ 
            hyphenate <$> (unpack <$> toList allowedlist)

        hyphenate x = Data.Text.concat 
            [pack "'", pack x , pack "'"]

