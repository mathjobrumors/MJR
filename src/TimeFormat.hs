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

module TimeFormat where

import Data.Int
import Data.Text

data TimeFormat = Long | Short
    deriving (Show, Eq)

formatTime :: TimeFormat -> Int64 -> Text
formatTime format sec = let

    seconds sec = if short then "s" else " second" ++ plural sec
    minutes a = if short then "m" else " minute" ++ plural a
    hours   a = if short then "h" else " hour" ++ plural a
    days    a = if short then "d" else " day" ++ plural a
    weeks   a = if short then "w" else " week" ++ plural a
    months  a = if short then "m" else " month" ++ plural a
    years   a = if short then "y" else " year" ++ plural a

    in
    
    case (sec, div sec 60, div sec 3600, 
                             div sec 86400, div sec 604800,
                             div sec 2592000, div sec 31536000) of

    (a, 0, 0, 0, 0, 0, 0) -> pack $ show sec ++ seconds sec
    (_, a, 0, 0, 0, 0, 0) -> pack $ show a ++ minutes a
    (_, _, a, 0, 0, 0, 0) -> pack $ show a ++ hours a
    (_, _, _, a, 0, 0, 0) -> pack $ show a ++ days a
    (_, _, _, _, a, 0, 0) -> pack $ show a ++ weeks a
    (_, _, _, _, _, a, 0) -> pack $ show a ++ months a
    (_, _, _, _, _, _, a) -> pack $ show a ++ years a

    where plural a  | a == 1 = ""
                    | otherwise = "s"

          short   = format == Short
