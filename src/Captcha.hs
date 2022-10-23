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

{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE GADTs                           #-}
{-# LANGUAGE MultiParamTypeClasses     	     #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE TypeFamilies                    #-}
{-# LANGUAGE DerivingStrategies		     #-}
{-# LANGUAGE UndecidableInstances 	     #-}
{-# LANGUAGE DataKinds 			     #-}
{-# LANGUAGE FlexibleInstances 		     #-}

module Captcha where

import           Data.Text (Text, pack, take, concat, intercalate, unpack)
import           Yesod (Entity(..), runDB, getBy, insert_)
import           Yesod.Core
import           Text.StringRandom (stringRandomIO)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.ByteString (ByteString, length)
import           Data.ByteString.Lazy (toStrict, fromStrict)

import           MainConfig
import           Auth (checkAuth, grantAuth, getAuthToken)

import           System.Random (randomRIO, randomIO)
import           Data.Binary (encode)
import           Data.Char (chr, ord)
import           Crypto.Hash.SHA256 as SHA256 (hash)

import           Data.ByteString.Internal (unsafeCreate)
import           Foreign.Ptr (plusPtr)
import           Foreign.Storable (poke)
import           Data.ByteString.Unsafe (unsafeIndex)
import           Data.Bits (shiftR, (.&.))
import           Data.Word (Word8)
import           Data.Hashable (hash)
import           RateLimiting (rateLimiting)


-- Number of Captcha in the database
captchaMaxId :: Int
captchaMaxId = 5

-- Target dimension 
finalSize :: (Int, Int)
finalSize = (64, 32)

-- Captcha dimensions 
captchaFrame :: (Int, Int)
captchaFrame = (300, 250)

-- We can implement rate limiting directly on the captcha
-- Note that we cannot alter the CFRS at the captcha stage
-- that would make submitting the comment impossible after a failure

generateCaptchaUrl :: Handler (Route MathJobsApp)
generateCaptchaUrl = do
    nonceID <- getAuthToken "captcha"
    return $ CaptchaR $ sha256sum $ nonceID

-- Cache it?

getCaptchaR :: Text -> Handler Html
getCaptchaR v = do
    nonce <- getAuthToken "captcha"
    let newr = 1 + (Data.Hashable.hash nonce `mod` (captchaMaxId - 1))
    captcha <- runDB $ getBy $ UniqueCaptchaID newr
    case captcha of
      Nothing -> do invalidArgs ["database error 1"]
      Just (Entity id cap) -> do
          let xpos = captchaDBCaptchaPosX cap
          let ypos = captchaDBCaptchaPosY cap
          setSessionBS "xpos" (toStrict $ encode xpos)
          setSessionBS "ypos" (toStrict $ encode ypos)
          replaceOrAddHeader "Cache-Control" "max-age=604800"
          sendResponse $ TypedContent typePng $ toContent $ captchaDBCaptchaImg cap

sha256sum :: Text -> Text
sha256sum v = decodeUtf8 $ toHex $ SHA256.hash $ encodeUtf8 v

maxLen :: Int
maxLen = maxBound `quot` 2

hexDig :: Word8 -> Word8
hexDig d
    | d < 10    = d + 48
    | otherwise = d + 87

toHex :: ByteString -> ByteString
toHex bs
    | len > maxLen = error "too long to convert"
    | otherwise    = unsafeCreate nl (go 0)
      where
        len = Data.ByteString.length bs
        nl  = 2*len
        go i p
            | i == len  = return ()
            | otherwise = case unsafeIndex bs i of
                            w -> do poke p (hexDig $ w `shiftR` 4)
                                    poke (p `plusPtr` 1) (hexDig $ w .&. 0xF)
                                    go (i+1) (p `plusPtr` 2)

captchaSize :: Int
captchaSize = 192

