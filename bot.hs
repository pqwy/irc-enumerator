{-# LANGUAGE OverloadedStrings, ViewPatterns, BangPatterns #-}

module Main where

import Network.IRC.Enumerator.Protocol
import Network.IRC.Enumerator.Message
import Network.IRC.Enumerator.Commands as C
import Network.IRC.Enumerator.Network

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import qualified Data.Text as T

import Control.Category ( (>>>), (<<<) )

import System.Environment

import Control.Concurrent
import Control.Exception
import Control.Monad


bot :: (String, Maybe Int) -> T.Text -> [T.Text] -> IO ()
bot host nick channels =
    ( E.run $ enumIRC host handshake $
        EL.zip (E.printChunks True)
        . reaction (return . f nick) ) >>= print

  where

    handshake = [ C.nick nick, C.user "hbotherder" 0 ".", C.join channels Nothing ]

    f me (Message (User nick _ _) (Command PRIVMSG) [to, msg])
        | to == me = [ C.privmsg nick msg ]
    f _ _ = []

main = do
    args <- getArgs

    case args of
         (server : port : nick : channels) ->
            bot (server, p) (T.pack nick) (map T.pack channels)
            where p | port == "_" = Nothing
                    | otherwise   = Just (read port)

         _ -> do
             me <- getProgName
             putStrLn $ "Usage: " ++ me ++ " <server> (<port> | _) <nick> <channel> [<channel> ...]"
