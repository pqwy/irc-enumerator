module Network.HIRC.Message (
    Message (..), Who (..), Command (..), Reply (..), Error (..)
) where

import Network.HIRC.Message.Interp 

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

data Message = Message Who Command [Text]
    deriving Show

data Who = Nobody
         | Server ByteString
         | User ByteString ByteString ByteString
    deriving Show

