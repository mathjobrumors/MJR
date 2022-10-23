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

{-# LANGUAGE RecordWildCards			    #-}
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
{-# LANGUAGE DataKinds 			    	    #-}
{-# LANGUAGE FlexibleInstances 		    	#-}

module TextValidate where

import MarkdownParsing (text2Html)
import TextParsing (isAllowed)
import Data.Text (Text(..), concat, pack, take, toLower)
import Text.Blaze.Internal 
import Text.Blaze.Html (Html(..))
import Tree (Tree(..))
import Data.Set (Set(..))

data TextValidator = TextValidator {
    description :: Text, 
    cutoff      :: Int, 
    allowedlist :: Set Text,
    domainslist :: Tree Text
} deriving (Show)

validatorSettings allowedlist domainslist = 
    TextValidator {   
        description = "", 
        cutoff = 80,
        allowedlist = allowedlist,
        domainslist = domainslist
    }

checkValid :: TextValidator -> Maybe Text -> Either Text (Html, Text)
checkValid settings Nothing = Left "corrupted message"

checkValid settings (Just x) = let 
    parsed  = text2Html (domainslist settings) (allowedlist settings) 
        $ Data.Text.take (cutoff settings) x
    in
        case parsed of
            (Just (Empty ())) -> 
                Left $ Data.Text.concat ["empty ", description settings]

            (Just text) -> Right (text, x)
            
            Nothing   -> Left "markup is restricted, see instructions \
                                    \at the bottom of the page"

