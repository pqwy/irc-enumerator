{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Enumerator.IO (
--     toMessage, enumIRC, enumIRCRaw
) where

import Network.IRC.Enumerator.Message
import Network.IRC.Enumerator.Message.Interp 

import Control.Applicative
import Control.Arrow ( (&&&) )
import Control.Category ( (>>>) )

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Monoid

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import           Data.Text ( Text )
import           Data.Text.Encoding       ( decodeUtf8With )
import           Data.Text.Encoding.Error ( lenientDecode )

import           Data.Enumerator ( Iteratee, Enumeratee, joinI, ($$), (>>==) )
import qualified Data.Enumerator      as E hiding ( map, head )
import qualified Data.Enumerator.List as E ( map, head, concatMapAccum )

import System.IO ( hPutStrLn, stderr )


toMessage :: ByteString -> Maybe Message
toMessage = either (\_ -> Nothing) Just . parseOnly pmessage

pmessage :: Parser Message
pmessage = Message
           <$> pwho
           <*> (skipSpace *> (interp Command Reply Error <$> notSp))
           <*> ( (dec.) . (++)
                 <$> (skipSpace *> many (notSpNor ':' <* skipSpace))
                 <*> ((:[]) <$> (char ':' *> takeByteString) <|> pure []))

pwho :: Parser Who
pwho = char ':' *>
        ( User <$> takeWhile1 isUserName
               <*> (char '!' *> notSpNor '@')
               <*> (char '@' *> notSp)
        <|> Server <$> notSp )
      <|> pure Nobody
  where
    isUserName c = any ($c) [ isAlpha_ascii, isDigit, inClass "[]\\`^{}|-]" ]

notSp :: Parser ByteString
notSp = takeWhile1 (not . isSpace)

notSpNor :: Char -> Parser ByteString
notSpNor x = takeWhile1 (\c -> not (isSpace c) && c /= x)

dec :: [ByteString] -> [Text]
dec = map (decodeUtf8With lenientDecode)

enumBlocks :: (Monoid a, Monad m) => (a -> (Maybe a, [b])) -> Enumeratee a b m t
enumBlocks blockf = E.concatMapAccum ((blockf.) . push) Nothing
  where
    push ma a = maybe a (`mappend` a) ma

splitOn :: ByteString -> ByteString -> [ByteString]
splitOn delim str =
    case delim `BS.breakSubstring` str of
         (pre, ""  ) -> [ pre ]
         (pre, post) -> pre : delim `splitOn` BS.drop (BS.length delim) post

enumLines :: (Monad m) => Enumeratee ByteString ByteString m t
enumLines = enumBlocks (((Just . last) &&& init) . splitOn "\r\n")

enumIRCRaw :: (Monad m) => Enumeratee ByteString (Either ByteString Message) m t
enumIRCRaw step = joinI $ enumLines $$ E.map decode step
  where
    decode bs = Left bs `maybe` Right $ toMessage bs

split :: (Monad m) => (a -> m t1) -> Enumeratee (Either a b) b m t2
split onErr s@(E.Continue k1)
    = E.head >>= \blk ->
        case blk of
             Nothing        -> return s
             Just (Left a)  -> lift (onErr a) >> split onErr s
             Just (Right b) -> k1 (E.Chunks [b]) >>== split onErr
split onErr s = return s

enumIRCWith :: (Monad m) => (ByteString -> m a) -> Enumeratee ByteString Message m t
enumIRCWith onErr step = joinI $ enumIRCRaw $$ split onErr step

enumIRC :: (MonadIO m) => Enumeratee ByteString Message m t
enumIRC = enumIRCWith $ \bs ->
            liftIO $ hPutStrLn stderr $ "[warning] unparsed line: " ++ show bs

