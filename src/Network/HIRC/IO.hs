module Network.HIRC.Message.IO (
    line
) where

import Network.HIRC.Message
import Network.HIRC.Message.Interp 

import Control.Applicative

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)


line :: ByteString -> Maybe Message
line = either (\_ -> Nothing) Just . parseOnly message

message :: Parser Message
message = Message
          <$> who
          <*> (interp <$> (skipSpace *> notSp))
          <*> ( (dec.) . (++)
                <$> (skipSpace *> many (notSpNor ':' <* skipSpace))
                <*> ((:[]) <$> (char ':' *> takeByteString) <|> pure []))

who :: Parser Who
who = ( char ':' *>
        ( notSpNor '!' >>= \pre ->
            (char '!' *> (User pre <$> notSpNor '@' <*> (char '@' *> notSp)))
                <|> pure (Server pre) )
        ) <|> pure Nobody

notSp :: Parser ByteString
notSp = takeWhile1 (not . isSpace)

notSpNor :: Char -> Parser ByteString
notSpNor x = takeWhile1 (\c -> not (isSpace c) && c /= x)

dec :: [ByteString] -> [Text]
dec = map (decodeUtf8With lenientDecode)

