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

module Redirect where
import Data.Text (pack, concat)
import Network.Wai 
import Data.List
import Data.ByteString (ByteString(..))
import Network.HTTP.Types.Method
import Data.Text.Encoding
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header


requestToAddr :: Request -> ByteString
requestToAddr req 
    | requestMethod req == methodGet = 
        encodeUtf8 $ Data.Text.concat [pack "https://www.mathjobrumors.com/", 
            Data.Text.concat $ Data.List.intercalate [pack "/"] $ fmap (\x -> [x]) $ pathInfo req]
    | otherwise = "https://www.mathjobrumors.com"

redirectApp :: Application
redirectApp req respond = 
    respond $ responseLBS status308 [(hLocation, requestToAddr req)] ""