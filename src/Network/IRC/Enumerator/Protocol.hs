{-# LANGUAGE ViewPatterns, DeriveDataTypeable, OverloadedStrings #-}

module Network.IRC.Enumerator.Protocol (
   toMessage, decode, fromMessage
) where

import Network.IRC.Enumerator.Message
import Network.IRC.Enumerator.Message.Command 

import Control.Monad
import Control.Applicative
import Control.Arrow ( (&&&) )
import Control.Category ( (>>>) )

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Monoid
import Data.Maybe

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Text.Encoding       ( decodeUtf8With, encodeUtf8 )
import           Data.Text.Encoding.Error ( lenientDecode )

import           Data.Enumerator ( Iteratee, Enumeratee, ($$), (>>==), (=$) )
import qualified Data.Enumerator      as E hiding ( map, head )
import qualified Data.Enumerator.List as E ( map, head, concatMapAccum )

import Control.Exception
import Data.Typeable ( Typeable )


--
-- Raw parsing
--

-- Parse a single line of IRC into a message. The line is expected to be
-- stripped of its CRLF delimiter.
-- 
toMessage :: ByteString -> Maybe Message
toMessage = either (\_ -> Nothing) Just . parseOnly pmessage

pmessage :: Parser Message
pmessage = Message
           <$> pwho
           <*> (skipSpace *> ppurp)
           <*> ( (map dec.) . (++)
                 <$> (skipSpace *> many (notSpNor ':' <* skipSpace))
                 <*> ((:[]) <$> (char ':' *> takeByteString) <|> pure []))

pwho :: Parser Who
pwho = char ':' *>
        ( User <$> (dec <$> takeWhile1 isUserName)
               <*> (dec <$> (char '!' *> notSpNor '@'))
               <*> (dec <$> (char '@' *> notSp))
        <|> (Server . dec) <$> notSp )
      <|> pure Nobody
  where
    isUserName c = any ($ c) [ isAlpha_ascii, isDigit, inClass "[]\\`^{}|-" ]

ppurp :: Parser Purpose
ppurp = (special <$> ( do I n <- number
                          return (fromInteger n)) )
        <|> (cmd <$> notSp)
  where
    special n = force ($ n)
                [ fmap Reply . knownReply
                , fmap Error . knownError
                , Just . Reply . OtherReply ]
    cmd b     = Command $ force ($ b) [ knownCommand , Just . C . dec ]

force :: (a -> Maybe b) -> [a] -> b
force f = fromJust . msum . map f

notSp :: Parser ByteString
notSp = takeWhile1 (not . isSpace)

notSpNor :: Char -> Parser ByteString
notSpNor x = takeWhile1 (\c -> not (isSpace c) && c /= x)

dec :: ByteString -> Text
dec = decodeUtf8With lenientDecode

--
-- Raw encoding.
-- 

-- XXX Sometimes the last arg needn't be :-quoted. Other times it needs to be.
-- XXX the "XXX" in the output requires bimaps.
fromMessage :: Message -> ByteString
fromMessage (Message who purpose args)
    = encodeUtf8 $ mconcat [ prefix who, command purpose, rest args, "\r\n" ]
  where
    prefix Nobody       = ""
    prefix (Server s)   = s                           |+| " "
    prefix (User a b c) = mconcat [a, "!", b, "@", c] |+| " "

    command (Command (C cmd)) = cmd               |+| " "
    command (Command cmd)     = T.pack (show cmd) |+| " "
    command _                 = "XXX "

    rest args = case (args, init args, last args) of
                     ([], _ , _) -> ""
                     (_ , [], a) -> ":" |+| a
                     (_ , as, a) -> T.unwords as |+| " :" |+| a

(|+|) :: (Monoid a) => a -> a -> a
(|+|) = mappend

--
-- Enumeratee input.
-- 

data CantParse = CantParse ByteString
    deriving (Typeable, Show)

instance Exception CantParse

-- Enumeratee from continuous ByteString input to a sequence of IRC messages.
-- If a line can't be parsed, stops with an error, because we're reading serious
-- gibberish.
-- 
decode :: (Monad m) => Enumeratee ByteString Message m t
--  decode = enumCrLfLines =$= parse'  -- enumerator 0.4.18
decode step = enumCrLfLines =$ parse' step

parse' :: (Monad m) => Enumeratee ByteString Message m t
parse' s@(E.Continue k)
    = E.head >>= \chunk ->
        case chunk of
             Nothing                      -> return s
             Just (toMessage -> Just msg) -> k (E.Chunks [msg]) >>== parse'
             Just bs                      -> E.throwError (CantParse bs)
parse' s = return s

-- Split incoming data on CRLF, as per IRC.
-- 
enumCrLfLines :: (Monad m) => Enumeratee ByteString ByteString m t
enumCrLfLines = enumBlocks (((Just . last) &&& init) . splitOn "\r\n")

enumBlocks :: (Monoid a, Monad m) => (a -> (Maybe a, [b])) -> Enumeratee a b m t
enumBlocks blockf = E.concatMapAccum ((blockf.) . push) Nothing
  where
    push ma a = maybe a (`mappend` a) ma

splitOn :: ByteString -> ByteString -> [ByteString]
splitOn delim str =
    case delim `BS.breakSubstring` str of
         (pre, ""  ) -> [ pre ]
         (pre, post) -> pre : delim `splitOn` BS.drop (BS.length delim) post

