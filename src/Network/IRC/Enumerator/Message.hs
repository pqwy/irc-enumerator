module Network.IRC.Enumerator.Message (
    Message (..), Purpose (..), Who (..), Command (..), Reply (..), Error (..)
  , message
) where

import Network.IRC.Enumerator.Message.Interp 

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

data Message = Message Who Purpose [Text]
    deriving Show

data Purpose = Command Command
             | Reply Reply
             | Error Error
    deriving Show

data Who = Nobody
         | Server ByteString
         | User ByteString ByteString ByteString
    deriving Show

message :: Command -> [Text] -> Message
message = Message Nobody . Command

