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

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies		      #-}
{-# LANGUAGE UndecidableInstances 	    #-}
{-# LANGUAGE DataKinds 			            #-}
{-# LANGUAGE FlexibleInstances 		      #-}
{-# LANGUAGE BangPatterns               #-}

module TextParsing where

import           Data.Text (Text, pack, take, concat, intercalate, words, toLower, unpack, length, drop, dropWhile)
import           Data.List (intercalate)
import           Data.Functor.Identity (Identity(..))
import           Data.Text.Lazy (toStrict)
import           Text.Parsec (ParsecT(..), string, many1, parse, digit, char, alphaNum, choice, try, anyChar, (<|>), manyTill)
import           Network.URI (parseURI, uriAuthority, uriRegName)
import           Text.Blaze.Html5 (Html, toHtml, toMarkup, a, (!), textValue)
import           Text.Blaze.Html5.Attributes (href, rel)
import           Data.Text (Text, pack, take, concat, intercalate)
import           Data.Functor.Identity (Identity(..))
import           Text.Blaze.Html (Html(..), toHtml, toMarkup, preEscapedToHtml)
import           Text.TeXMath (DisplayType(..), readTeX, writeMathML)
import           Text.Parsec (parse, string, ParsecT(..), manyTill, char, anyChar, lookAhead, anyToken, notFollowedBy, try, (<|>))
import           Text.XML.Light.Output (showElement)
import           Debug.Trace (trace)
import           Data.Sort (sort)
import           Tree
import           IpParse
import           Data.Set as Set (member, Set(..), unions, fromList, toList)

text2MathMl d v =
  case parse (parseMath d) "" v of
    Right u -> toHtml $ toMarkup <$> u
    Left u -> toHtml $ pack "[erroneous latex]"

parseMath :: Tree Text -> ParsecT Text () Identity [Html]
parseMath d = do
  manyTill equParse (notFollowedBy anyToken)

  where

    equParse = try displayEquation <|> (try inlineEquation <|> (try text <|> try textEnd))

    textEnd = do
      equ <- manyTill anyChar (notFollowedBy anyToken)
      return $ transformString d equ

    text = do
      equ <- manyTill anyChar (lookAhead $ char '$')
      return $ transformString d equ

    inlineEquation = do
      char '$'
      equ <- manyTill anyChar (char '$')
      return $ writeXML DisplayInline equ

    displayEquation = do
      Text.Parsec.string "$$"
      equ <- manyTill anyChar (Text.Parsec.string "$$")
      return $ writeXML DisplayBlock equ

writeXML blocktype equ = case readTeX $ pack equ of
    Right v ->
      preEscapedToHtml $ pack $ showElement $ writeMathML blocktype v
    Left _ ->
        toHtml $ pack equ

-- Assumes Text is all lower case

getUrls :: Tree Text -> Text -> [Words]
getUrls v t = Prelude.filter (hasUrl v) (getWords $ unpack t)

isAllowed :: Tree Text -> Set Text -> Text -> Bool
isAllowed tree d text = 
  if hasIp (toLower text) then 
    False
  else
    trace (show (getUrls tree (toLower text))) 
      and $ fmap (\x -> isAllowed' d x) $ getUrls tree (toLower text)

isAllowed' :: Set Text -> Words -> Bool
isAllowed' d (Space _) = False -- should never happen anyway
isAllowed' d (Word t) = 
  let 
    parsed = toLower . pack <$> extractDomain (unpack t)
  in
    case parsed of
      Just x -> x `Set.member` d
      Nothing -> False

data Words = Word Text | Space Int | Newline | Nil
    deriving Show

getWords :: String -> [Words]
getWords (x:xs) = getWords' Nil (x:xs)
getWords [] = []

getWords' v ('\r':xs)         = getWords' v xs -- skip \r
getWords' Nil ('\n':xs)       = getWords' Newline xs
getWords' Nil (' ':xs)        = getWords' (Space 1) xs
getWords' Nil (x:xs)          = getWords' (Word $ pack [x]) xs
getWords' (Word a) ('\n':xs)  = (Word a:Newline:(getWords' Nil xs))
getWords' (Word a) (' ':xs)   = (Word a):(getWords' (Space 1) xs)
getWords' (Word a) (x:xs)     = getWords' (Word $ Data.Text.concat [a, pack $ [x]]) xs 
getWords' (Space n) ('\n':xs) = (Space n:Newline:getWords' Nil xs)
getWords' (Space n) (' ':xs)  = getWords' (Space (n + 1)) xs
getWords' (Space n) (x:xs)    = (Space n):(getWords' (Word $ pack [x]) xs)
getWords' (Word a) ['\n']     = [Word a, Newline]
getWords' (Word a) [' ']      = [Word a, Space 1]
getWords' (Word a) [x]        = [Word $ Data.Text.concat [a, pack [x]]]
getWords' Newline xs          = Newline:(getWords' Nil xs) -- Should never arise
getWords' Nil []              = []
getWords' acc []              = [acc]

-- Detect matches with Tree after . symbol

searchTextAfterDots :: Tree Text -> Text -> Bool
searchTextAfterDots t str 
    | str == pack "" = False
    | otherwise = let 
            (rem, b) = searchTextInTree (Data.Text.drop 1 $ Data.Text.dropWhile (\x -> x /= '.') str) t
        in 
            if b then True
            else searchTextAfterDots t rem

transformString :: Tree Text -> String -> Html
transformString d u = toMarkup $ transform d <$> getWords u

    where

        transform :: Tree Text -> Words -> Html
        transform d (Space n) = toHtml $ pack " "
        transform d Newline = toHtml $ pack "\n"
        transform d (Word t) | hasUrl d (Word t) = a ! href (textValue t) ! rel "noreferrer" $ toHtml t
                             | otherwise = toHtml $ t

extractDomain :: String -> Maybe String
extractDomain t = do
    u <- parseURI t
    w <- uriAuthority u
    return $ uriRegName w

hasUrl :: Tree Text -> Words -> Bool
hasUrl t (Word txt) = searchTextAfterDots t txt 
hasUrl _ (Newline) = False
hasUrl _ (Space n) = False
hasUrl _ Nil = False
  