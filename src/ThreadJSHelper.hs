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

module ThreadJSHelper where

import Data.Text (pack, concat)
import MainConfig
import Text.Blaze.Html (preEscapedToHtml, Html(..))

tokenize (VUpR a b) = Data.Text.concat ["u", pack $ show a, "p", pack $ show b ]
tokenize (VDownR a b) = Data.Text.concat ["d", pack $ show a, "p", pack $ show b ]

voteJS val post token render = Data.Text.concat
              [ "vote(", "'", render (token (postThreadId post) (postPostId post)), "'",
                ",", isUpButton (token 0 0),
                ",", binarize (val `elem` postUpVotedBy post),
                ",", binarize (val `elem` postDownVotedBy post),
                ",", pack $ show (postUpVotes post),
                ",", pack $ show (postDownVotes post),
                ",", "'", tokenize (VUpR (postThreadId post) (postPostId post)), "'",
                ",", "'", tokenize (VDownR (postThreadId post) (postPostId post)), "'", ")" ]

    where binarize True = pack "1"
          binarize False = pack "0"
          isUpButton (VUpR _ _) = pack "1"
          isUpButton (VDownR _ _) = pack "0"
          isUpButton _ = pack "-1"

reportJS val post token render =
    Data.Text.concat 
        [ "report('", 
           render (token (postThreadId post) (postPostId post)), 
           "')" ]
        