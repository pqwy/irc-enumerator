{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Network.IRC.Enumerator.Network (
    enumIRC, enumIRC', iterMessages
) where

import Network.IRC.Enumerator.Message
import Network.IRC.Enumerator.Protocol
import qualified Network.IRC.Enumerator.Commands as C

import Control.Monad
import Control.Applicative
import Data.Functor
import Data.List ( intersperse )
import Data.Maybe
import Data.Monoid

import qualified Data.ByteString.Char8 as BS

import           Data.Enumerator ( Iteratee, Enumeratee, ($$), (>>==), ($=), (=$) )
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NS
import qualified Network.Socket.Enumerator as NSE


connect :: (MonadIO m) => NS.HostName -> Maybe Int -> Iteratee a m NS.Socket
connect host port = E.tryIO $ do
    (addr:_) <- NS.getAddrInfo (Just hints) (Just host) (show <$> port')
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr)
                      (NS.addrProtocol addr)
    NS.connect sock (NS.addrAddress addr)
    return sock
  where
    hints = NS.defaultHints { NS.addrFlags      = [NS.AI_ADDRCONFIG]
                            , NS.addrSocketType = NS.Stream }
    port' = port `mplus` Just 6667

finallyE :: (Monad m) => Iteratee a m b -> Iteratee a m c -> Iteratee a m b
finallyE a1 a2 = (a1 `E.catchError` \e -> a2 >> E.throwError e) <* a2

iterMessages :: (MonadIO m) => NS.Socket -> Iteratee Message m ()
iterMessages socket = EL.map fromMessage =$ NSE.iterSocket socket

enumIRC' :: MonadIO m => (NS.Socket -> Iteratee Message m b) -> NS.HostName -> Maybe Int -> Iteratee BS.ByteString m b
enumIRC' consumer host port = do
    sock <- connect host port
    -- XXX FIXME does not catch hard async exceptions, needs monadcontrol
    ( NSE.enumSocket 4096 sock $$ decode =$ consumer sock )
        `finallyE` liftIO (NS.shutdown sock NS.ShutdownBoth)

enumIRC :: (MonadIO m)
        => (Iteratee Message m () -> Iteratee Message m b)
        -> [Message]
        -> NS.HostName
        -> Maybe Int
        -> Iteratee BS.ByteString m b
enumIRC consumer handshake = enumIRC' $ \sock ->
    EL.zipWith (flip const) (housekeep $$ iterMessages sock)
                            (consumer (iterMessages sock))
  where
    housekeep = E.checkContinue0 $ \_ out -> pong $ out (E.Chunks handshake)

pong :: Monad m => Iteratee Message m () -> Iteratee Message m ()
--  pong :: MonadIO m => Iteratee Message m () -> Iteratee Message m ()
pong out = EL.concatMap f =$ out
--  pong out = ( joinI $ EL.concatMap f $$ EL.zip out (E.printChunks True) ) >> return ()
  where
    f (Message _ (Command PING) (s1:ss)) = [ C.pong s1 (listToMaybe ss) ]
    f _                                  = []

--  react :: (MonadIO m) => 

