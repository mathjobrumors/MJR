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


module IpParse where

import Data.Attoparsec.Text
import Data.Text

findIp :: Parser [Char]
findIp = manyTill anyChar ip

  where

    ip = do 
      manyTill digit $ char '.'
      manyTill digit $ char '.'
      manyTill digit $ char '.'
      digit 
      return () 

hasIp :: Text -> Bool
hasIp t = case parseOnly findIp t of
    Right _ -> True
    Left  _ -> False