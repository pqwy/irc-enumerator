{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Enumerator.Commands
where


import Network.IRC.Enumerator.Message

import Data.List ( intersperse )
import Data.Text ( Text )


class Param p where params :: p -> [Text]

instance Param Text where params t = [t]

instance Param Int where params = undefined

instance (Param p) => Param (Maybe p) where params = maybe [] params

instance (Param p) => Param [p] where
    params = intersperse "," . (params =<<)

class CmdBuilder c where msg :: Command -> ([Text] -> [Text]) -> c

instance CmdBuilder Message where
    msg c args = Message Nobody (Command c) (args [])

instance (Param p, CmdBuilder c) => CmdBuilder (p -> c) where
    msg cmd args p = msg cmd (args . (params p ++))

m :: (CmdBuilder c) => Command -> c
m = (`msg` id)


admin, away, info :: Maybe Text -> Message
admin = m ADMIN
away  = m AWAY
info  = m INFO


ping, pong :: Text -> Maybe Text -> Message
ping = m PING
pong = m PONG


privmsg, invite :: Text -> Text -> Message
privmsg = m PRIVMSG
invite  = m INVITE

quit :: Maybe Text -> Message
quit = m QUIT


--  --  Defined in RFC 1459
--  --  [edit] CONNECT

--  --  Syntax:

--  --      CONNECT <target server> [<port> [<remote server>]] (RFC 1459)
--  --      CONNECT <target server> <port> [<remote server>] (RFC 2812)

--  --  Instructs the server <remote server> (or the current server, if <remote server> is omitted) to connect to <target server> on port <port>.[3][4] This command should only be available to IRC Operators.

die :: Message
die = m DIE 

nick, error_ :: Text -> Message
nick   = m NICK
error_ = m ERROR

ison :: [Text] -> Message
ison = m ISON

join :: [Text] -> Maybe [Text] -> Message
join = m JOIN

kick :: Text -> Text -> Maybe Text -> Message
kick = m KICK

kill :: Text -> Text -> Message
kill = m KILL

--  links 
--  Syntax:

--      LINKS [<remote server> [<server mask>]]

--  Lists all server links matching <server mask>, if given, on <remote server>, or the current server if omitted.[13]

--  [edit] LIST

--  Syntax:

--      LIST [<channels> [<server>]]

--  Lists all channels on the server.[14] If the comma-separated list <channels> is given, it will return the channel topics. If <server> is given, the command will be forwarded to <server> for evaluation.

--  [edit] LUSERS

--  Syntax:

--      LUSERS [<mask> [<server>]]

--  Returns statistics about the size of the network.[15] If called with no arguments, the statistics will reflect the entire network. If <mask> is given, it will return only statistics reflecting the masked subset of the network. If <target> is given, the command will be forwarded to <server> for evaluation.

--  Defined in RFC 2812
--  [edit] MODE

--  Syntax:

--      MODE <nickname> <flags> (user)
--      MODE <channel> <flags> [<args>]

--  The MODE command is dual-purpose. It can be used to set both user and channel modes.[16]

--  Defined in RFC 1459

motd :: Maybe Text -> Message
motd = m MOTD

--  Defined in RFC 2812
--  [edit] NAMES

--  Syntax:

--      NAMES [<channels>] (RFC 1459)
--      NAMES [<channels> [<server>]] (RFC 2812)

--  Returns a list of who is on the comma-separated list of <channels>, by channel name.[18] If <channels> is omitted, all users are shown, grouped by channel name with all users who are not on a channel being shown as part of channel "*". If <server> is specified, the command is sent to <server> for evaluation.[19]

--  Defined in RFC 1459; the optional <server> parameter was added in RFC 2812
--  [edit] NICK


notice, oper :: Text -> Text -> Message
notice = m NOTICE
oper   = m OPER

part :: [Text] -> Message
part = m PART

pass :: Text -> Message
pass = m PASS

rehash, restart :: Message
rehash = m REHASH
restart = m RESTART

--  [edit] SERVICE

--  Syntax:

--      SERVICE <nickname> <reserved> <distribution> <type> <reserved> <info>

--  Registers a new service on the network.[32]

--  Defined in RFC 2812
--  [edit] SERVLIST

--  Syntax:

--      SERVLIST [<mask> [<type>]]

--  Lists the services currently on the network.[33]

--  Defined in RFC 2812
--  [edit] SERVER

server :: Text -> Int -> Text -> Message
server = m SERVER

squery, squit :: Text -> Text -> Message
squery = m SQUERY
squit  = m SQUIT

stats :: Text -> Maybe Text -> Message
stats = m STATS

--  Defined in RFC 1459
--  [edit] SUMMON

--  Syntax:

--      SUMMON <user> [<server>] (RFC 1459)
--      SUMMON <user> [<server> [<channel>]] (RFC 2812)

--  Gives users who are on the same host as <server> a message asking them to join IRC.[38][39]

--  Defined in RFC 1459; the optional <channel> parameter was added in RFC 2812

time :: Maybe Text -> Message
time = m TIME

topic :: Text -> Maybe Text -> Message
topic = m TOPIC

--  Defined in RFC 1459
--  [edit] TRACE

--  Syntax:

--      TRACE [<target>]

--  Trace a path across the IRC network to a specific server or client, in a similar method to traceroute.[42]

--  Defined in RFC 1459
--  [edit] USER

user :: Text -> Int -> Text -> Message
user x _ = m USER x ("0" :: Text) ("X" :: Text)

--  Syntax:

--      USER <username> <hostname> <servername> <realname> (RFC 1459)
--      USER <user> <mode> <unused> <realname> (RFC 2812)

--  This command is used at the beginning of a connection to specify the username, hostname, real name and initial user modes of the connecting client.[43][44] <realname> may contain spaces, and thus must be prefixed with a colon.

--  Defined in RFC 1459, modified in RFC 2812
--  [edit] USERHOST

--  Syntax:

--      USERHOST <nickname> [<nickname> <nickname> ...]

--  Returns a list of information about the nicknames specified.[45]

--  Defined in RFC 1459
--  [edit] USERS

--  Syntax:

--      USERS [<server>]

--  Returns a list of users and information about those users in a format similar to the UNIX commands who, rusers and finger.[46]

--  Defined in RFC 1459
--  [edit] VERSION

--  Syntax:

--      VERSION [<server>]

--  Returns the version of <server>, or the current server if omitted.[47]

--  Defined in RFC 1459
--  [edit] WALLOPS

--  Syntax:

--      WALLOPS <message>

--  Sends <message> to all operators connected to the server (RFC 1459), or all users with user mode 'w' set (RFC 2812).[48][49]

--  Defined in RFC 1459
--  [edit] WHO

--  Syntax:

--      WHO [<name> ["o"]]

--  Returns a list of users who match <name>.[50] If the flag "o" is given, the server will only return information about IRC Operators.

--  Defined in RFC 1459
--  [edit] WHOIS

--  Syntax:

--      WHOIS [<server>] <nicknames>

--  Returns information about the comma-separated list of nicknames masks <nicknames>.[51] If <server> is given, the command is forwarded to it for processing.

--  Defined in RFC 1459
--  [edit] WHOWAS

--  Syntax:

--      WHOWAS <nickname> [<count> [<server>]]

--  Used to return information about a nickname that is no longer in use (due to client disconnection, or nickname changes).[52] If given, the server will return information from the last <count> times the nickname has been used. If <server> is given, the command is forwarded to it for processing. In RFC 2812, <nickname> can be a comma-separated list of nicknames.[53]
