{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Network.IRC.Enumerator.Network (
    enumIRC, enumIRC', iterMessages, reaction
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

import           Data.Enumerator ( Iteratee, Enumeratee, Enumerator, ($$), (>>==), ($=), (=$) )
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NS
import qualified Network.Socket.Enumerator as NSE


connect :: (MonadIO m) => (NS.HostName, Maybe Int) -> Iteratee a m NS.Socket
connect (host, port) = E.tryIO $ do
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
iterMessages socket = encode =$ NSE.iterSocket socket

enumIRC' :: MonadIO m => (NS.HostName, Maybe Int) -> (NS.Socket -> Iteratee Message m b) -> Iteratee BS.ByteString m b
enumIRC' hp consumer = do
    sock <- connect hp
    ( NSE.enumSocket 4096 sock $$ decode =$ consumer sock )
        `finallyE` liftIO (NS.shutdown sock NS.ShutdownBoth)

enumIRC :: (MonadIO m)
        => (NS.HostName, Maybe Int)
        -> [Message]
        -> (Iteratee Message m () -> Iteratee Message m b)
        -> Iteratee BS.ByteString m b
enumIRC hp handshake consumer = enumIRC' hp $ \sock ->
    EL.zipWith (flip const) (housekeep (iterMessages sock))
                            (consumer (iterMessages sock))
  where
    housekeep out = pong $ enumList' handshake $$ out

reaction :: (Monad m) => (Message -> m [Message]) -> Iteratee Message m () -> Iteratee Message m ()
reaction f out = EL.concatMapM f =$ out

pong :: (Monad m) => Iteratee Message m () -> Iteratee Message m ()
pong = reaction (return . f)
  where
    f (Message _ (Command PING) (s1:ss)) = [ C.pong s1 (listToMaybe ss) ]
    f _                                  = []

-- The new regular enumList of enumerator-0.5
enumList' :: (Monad m) => [a] -> Enumerator a m b
enumList' as@(_:_) (E.Continue k) = k (E.Chunks as)
enumList' _        s              = E.returnI s

