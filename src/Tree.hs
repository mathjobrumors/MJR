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

module Tree where

import Data.Sort (sort)
import Prelude (init, otherwise, ($), filter, Show, Ord, Bool(..), (<=), (==), (>), (<), (.), (/=))
import Data.Text (take, drop, length, Text, dropWhile, pack)

-- Tree rapid search

data Tree b = Tree !b (Tree b) (Tree b) | Node !b
    deriving Show

-- Balanced tree construction
-- Not efficient but it doesn't matter
-- Used only once.

listToTree :: (Ord a) => [a] -> Tree a
listToTree [a] = Node a
listToTree [a,b] | a < b = Tree a (Node a) (Node b)
listToTree [a,b] | a > b = Tree b (Node b) (Node a)
listToTree [a,b] | a == b = Node a
listToTree list = 
  let 
    midel = median list
  in
    Tree midel 
      (listToTree $ filter (\x -> x <= midel) list) 
      (listToTree $ filter (\x -> x > midel) list)

medianFromSorted :: (Ord a) => [a] -> a
medianFromSorted [a] = a
medianFromSorted [a,b] = min a b
  where 
    min a b | a <= b = a 
            | otherwise = b
medianFromSorted (a:xs) = medianFromSorted (init xs) 

median :: Ord a => [a] -> a
median = medianFromSorted . sort

searchTextInTree :: Text -> Tree Text -> (Text, Bool)
searchTextInTree acc (Tree a t1 t2) 
  | trunc a acc > a = searchTextInTree acc t2
  | trunc a acc <= a = searchTextInTree acc t1

searchTextInTree acc (Node a) = (drop (length a) acc, trunc a acc == a)

trunc a acc = take (length a) acc
