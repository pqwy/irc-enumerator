module Network.IRC.Enumerator.Message (
    Message (..), Purpose (..), Who (..), Command (..), Reply (..), Error (..)
  , message
) where

import Network.IRC.Enumerator.Message.Command 

import Data.Text (Text)

data Message = Message Who Purpose [Text]
    deriving Show

data Purpose = Command Command
             | Reply Reply
             | Error Error
    deriving Show

data Who = Nobody
         | Server Text
         | User Text Text Text
    deriving Show

message :: Command -> [Text] -> Message
message = Message Nobody . Command

