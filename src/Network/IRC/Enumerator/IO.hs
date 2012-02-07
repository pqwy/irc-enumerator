{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Enumerator.IO (
--     toMessage, enumIRC, enumIRCRaw
) where

import Network.IRC.Enumerator.Message
import Network.IRC.Enumerator.Message.Interp 

import Control.Applicative
import Control.Arrow
import Control.Category ( (>>>) )

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Monoid

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Data.Enumerator ( Iteratee, Enumeratee, joinI, ($$), (>>==), (==<<), (>==>), (<==<) )
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import System.IO

import Debug.Trace


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
    isUserName = any' [isAlpha_ascii, isDigit, inClass "[]\\`^{}|-]"]

notSp :: Parser ByteString
notSp = takeWhile1 (not . isSpace)

notSpNor :: Char -> Parser ByteString
notSpNor x = takeWhile1 (\c -> not (isSpace c) && c /= x)

dec :: [ByteString] -> [Text]
dec = map (decodeUtf8With lenientDecode)

any' :: [a -> Bool] -> a -> Bool
any' = (getAny.) . mconcat . map (Any .)


blocked :: (Monoid a, Monad m) => (a -> (Maybe a, [b])) -> E.Enumeratee a b m t
blocked blockf = EL.concatMapAccum ((blockf.) . push) Nothing
  where
    push ma a = maybe a (`mappend` a) ma

lines' :: (Monad m) => E.Enumeratee Text Text m t
lines' = blocked (((Just . last) &&& init) . T.splitOn "\r\n")

splitOn :: ByteString -> ByteString -> [ByteString]
splitOn delim str =
    case delim `BS.breakSubstring` str of
         (pre, ""  ) -> [ pre ]
         (pre, post) -> pre : delim `splitOn` BS.drop (BS.length delim) post

lines'' :: (Monad m) => E.Enumeratee ByteString ByteString m t
lines'' = blocked (((Just . last) &&& init) . splitOn "\r\n")

enumIRCRaw :: (Monad m) => E.Enumeratee ByteString (Either ByteString Message) m t
enumIRCRaw step = joinI $ lines'' $$ EL.map decode step
  where
    decode bs = Left bs `maybe` Right $ toMessage bs

split :: (Monad m) => (a -> m t1) -> E.Enumeratee (Either a b) b m t2
split onErr s@(E.Continue k1) =
    EL.head >>= \blk ->
        case blk of
             Nothing        -> return s
             Just (Left a)  -> lift (onErr a) >> split onErr s
             Just (Right b) -> k1 (E.Chunks [b]) >>== split onErr
split onErr s = return s

enumIRCWith :: (Monad m) => (ByteString -> m a) -> E.Enumeratee ByteString Message m t
enumIRCWith onErr step = joinI $ enumIRCRaw $$ split onErr step

enumIRC :: (MonadIO m) => E.Enumeratee ByteString Message m t
enumIRC = enumIRCWith $ \bs ->
            liftIO $ hPutStrLn stderr $ "[warning] unparsed line: " ++ show bs

