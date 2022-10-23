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

{-# LANGUAGE FlexibleContexts			#-}
{-# LANGUAGE GADTs				#-}
{-# LANGUAGE MultiParamTypeClasses		#-}
{-# LANGUAGE OverloadedStrings			#-}
{-# LANGUAGE TypeFamilies			#-}
{-# LANGUAGE DerivingStrategies			#-}
{-# LANGUAGE UndecidableInstances		#-}
{-# LANGUAGE DataKinds 			        #-}
{-# LANGUAGE FlexibleInstances 		    	#-}

module MarkdownParsing where

import           Data.Text (Text, pack, take, concat, intercalate, unpack)
import           Yesod (toHtml)
import           Text.StringRandom (stringRandomIO)
import           Data.Time (secondsToDiffTime, getCurrentTime)
import           Text.Hamlet (Html)
import           Data.Functor.Identity (runIdentity)
import           CMark as C
import           Text.Blaze (toMarkup, text)
import           Text.Blaze.Html4.Strict as S (p, hr, blockquote,
                                         h2, h3, h4, h5, h6, ol, ul, li,
                                         br, b, code, em, span, a, string, i)
import           Text.Blaze.Html4.Strict.Attributes (href)
import           Text.Blaze.Renderer.Utf8 (renderMarkup)

import           TextParsing (text2MathMl, isAllowed)
import           Tree as T (Tree(..))
import           Data.Set (Set(..))

---------------------------
-- Text to HTML parsing ---
---------------------------

purifyMarkdown :: Text -> Text
purifyMarkdown t =
    nodeToCommonmark [] Nothing $ removeBlocks $ commonmarkToNode [] $ textPreFormat t

blockquoteMarkdown :: Text -> Text
blockquoteMarkdown t =
    case commonmarkToNode [] $ textPreFormat t of
        C.Node pos v s -> nodeToCommonmark [] Nothing $ C.Node pos v [C.Node pos BLOCK_QUOTE s]

node2Html :: T.Tree Text -> Set Text -> C.Node -> Maybe Text.Hamlet.Html
node2Html d allowed (C.Node a b s) =
    case b of
        
        DOCUMENT -> toMarkup <$> Prelude.traverse node2Html' s
        
        THEMATIC_BREAK -> Just hr
        
        PARAGRAPH -> p . toMarkup <$> Prelude.traverse node2Html' s
        
        BLOCK_QUOTE -> (blockquote <$> toHtml) . toMarkup <$> Prelude.traverse node2Html' s
        
        HTML_BLOCK h -> Nothing 

        CUSTOM_BLOCK _ _ -> Nothing 
        
        CODE_BLOCK info t -> 
                if isAllowed d allowed t then do
                    list <- Prelude.traverse node2Html' s
                    return $ toMarkup $ (S.p (S.code $ text t)) : list
                else Nothing

        HEADING level -> case level of
            1 -> h2 . toMarkup <$> Prelude.traverse node2Html' s
            2 -> h3 . toMarkup <$> Prelude.traverse node2Html' s
            3 -> h4 . toMarkup <$> Prelude.traverse node2Html' s
            4 -> h5 . toMarkup <$> Prelude.traverse node2Html' s
            _ -> h6 . toMarkup <$> Prelude.traverse node2Html' s
        
        LIST (ListAttributes BULLET_LIST _ _ _) -> ul . toMarkup <$> Prelude.traverse node2Html' s
        
        LIST (ListAttributes ORDERED_LIST _ _ _) -> ol . toMarkup <$> Prelude.traverse node2Html' s
        
        ITEM -> li . toMarkup <$> Prelude.traverse node2Html' s -- Item of list
        
        TEXT t ->
            if isAllowed d allowed t then do
                list <- Prelude.traverse node2Html' s
                return $ toMarkup $ text2MathMl d t : list
            else Nothing

        SOFTBREAK -> do
            list <- Prelude.traverse node2Html' s
            return $ toMarkup $ toHtml '\n' : list

        LINEBREAK -> do
            list <- Prelude.traverse node2Html' s
            return $ toMarkup $ toHtml '\n' : list
        
        HTML_INLINE t -> Nothing 
        
        CUSTOM_INLINE _ _ -> Nothing 
        
        CODE t -> 
            if isAllowed d allowed t then do
                list <- Prelude.traverse node2Html' s
                return $ toMarkup $ (S.code $ text t) : list
            else Nothing

        EMPH -> S.i . toMarkup <$> Prelude.traverse node2Html' s
        
        STRONG -> S.b . toMarkup <$> Prelude.traverse node2Html' s
        
        LINK url title -> Nothing 
        
        IMAGE _ _ -> Nothing 

    where node2Html' = node2Html d allowed

realFormat xs = preFormat xs Nothing

preFormat ('$':'$':xs) Nothing = '$' : '$' : preFormat xs (Just 2)
preFormat ('$':xs) Nothing = '$' : preFormat xs (Just 1)
preFormat ('\n':xs) a@(Just _) = preFormat xs a
preFormat (a:xs) v@(Just _) | a /= '$' = a : preFormat xs v
preFormat ('$':xs) (Just 2) = '$' : preFormat xs (Just 1)
preFormat ('$':xs) (Just 1) = '$' : preFormat xs Nothing
preFormat (a:xs) Nothing = a : preFormat xs Nothing
preFormat [] _ = []

textPreFormat = pack . realFormat . unpack

text2Html :: Tree Text -> Set Text ->  Text -> Maybe Text.Hamlet.Html
text2Html d allowed t = node2Html d allowed $ removeBlocks $ commonmarkToNode [] $ textPreFormat t

-- Eliminate nesting in excess of two.
-- Show only the two most recent nested blockquote. 

removeBlocks n = removeBlocks' 0 n
    where
    removeBlocks' counter (C.Node a b s) =
        case b of
        
            BLOCK_QUOTE  -> 
                if counter < 2 then 
                    C.Node a b (fmap (removeBlocks' (counter + 1)) s) 
                else C.Node a BLOCK_QUOTE [C.Node a (TEXT "[...]") []]

            _           -> C.Node a b (fmap (removeBlocks' counter) s)

removeBlocks node@(C.Node a b []) = C.Node a b []

formatInput :: Text -> Text
formatInput t =
    nodeToHtml [] $ removeBlocks $ commonmarkToNode [] $ textPreFormat t

