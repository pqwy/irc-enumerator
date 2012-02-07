{-# LANGUAGE OverloadedStrings #-}

module Network.HIRC.Message.Interp (
    interp, Command (..), Error (..), Reply (..)
) where

import Data.Monoid
import Data.Maybe
import Data.ByteString.Char8 ( ByteString, readInt, unpack )


interp :: (Command -> a) -> (Reply -> a) -> (Error -> a) -> ByteString -> a
interp cmd rep err str =
    case readInt str of
         Just (n, "") -> rep (reply n) `maybe` err $ error_ n
         _            -> cmd (command str)

data Command =
      ADMIN
    | AWAY
    | CONNECT
    | DIE
    | ERROR
    | INFO
    | INVITE
    | ISON
    | JOIN
    | KICK
    | KILL
    | LINKS
    | LIST
    | LUSERS
    | MODE
    | MOTD
    | NAMES
    | NICK
    | NOTICE
    | OPER
    | PART
    | PASS
    | PING
    | PONG
    | PRIVMSG
    | QUIT
    | REHASH
    | RESTART
    | SERVICE
    | SERVLIST
    | SERVER
    | SQUERY
    | SQUIT
    | STATS
    | SUMMON
    | TIME
    | TOPIC
    | TRACE
    | USER
    | USERHOST
    | USERS
    | VERSION
    | WALLOPS
    | WHO
    | WHOIS
    | WHOWAS
    | OtherCommand String
    deriving Show

command :: ByteString -> Command
command "ADMIN"    = ADMIN
command "AWAY"     = AWAY
command "CONNECT"  = CONNECT
command "DIE"      = DIE
command "ERROR"    = ERROR
command "INFO"     = INFO
command "INVITE"   = INVITE
command "ISON"     = ISON
command "JOIN"     = JOIN
command "KICK"     = KICK
command "KILL"     = KILL
command "LINKS"    = LINKS
command "LIST"     = LIST
command "LUSERS"   = LUSERS
command "MODE"     = MODE
command "MOTD"     = MOTD
command "NAMES"    = NAMES
command "NICK"     = NICK
command "NOTICE"   = NOTICE
command "OPER"     = OPER
command "PART"     = PART
command "PASS"     = PASS
command "PING"     = PING
command "PONG"     = PONG
command "PRIVMSG"  = PRIVMSG
command "QUIT"     = QUIT
command "REHASH"   = REHASH
command "RESTART"  = RESTART
command "SERVICE"  = SERVICE
command "SERVLIST" = SERVLIST
command "SERVER"   = SERVER
command "SQUERY"   = SQUERY
command "SQUIT"    = SQUIT
command "STATS"    = STATS
command "SUMMON"   = SUMMON
command "TIME"     = TIME
command "TOPIC"    = TOPIC
command "TRACE"    = TRACE
command "USER"     = USER
command "USERHOST" = USERHOST
command "USERS"    = USERS
command "VERSION"  = VERSION
command "WALLOPS"  = WALLOPS
command "WHO"      = WHO
command "WHOIS"    = WHOIS
command "WHOWAS"   = WHOWAS
command wat        = OtherCommand (unpack wat)

data Reply =
      TraceLink
    | TraceConnecting
    | TraceHandshake
    | TraceUnknown
    | TraceOperator
    | TraceUser
    | TraceServer
    | TraceNewtype
    | StatsLinkInfo
    | StatsCommands
    | StatsCLine
    | StatsNLine
    | StatsILine
    | StatsKLine
    | StatsYLine
    | EndOfStats
    | UModeIs
    | StatsLLine
    | StatsUpTime
    | StatsOLine
    | StatsHLine
    | StatsConn
    | LUserClient
    | LUserOp
    | LUserUnknown
    | LUserChannels
    | LUserMe
    | AdminMe
    | AdminLoc1
    | AdminLoc2
    | AdminEmail
    | TraceLog
    | LocalUsers
    | GlobalUsers
    | None
    | Away
    | UserHost
    | IsOn
    | Unaway
    | Nowaway
    | WhoIsUser
    | WhoIsServer
    | WhoIsOperator
    | WhoWasUser
    | EndOfWho
    | WhoIsIdle
    | EndOfWhois
    | WhoIsChannels
    | ListStart
    | List
    | ListEnd
    | ChannelModeIs
    | NoTopic
    | Topic
    | Inviting
    | Summoning
    | Version
    | WhoReply
    | NamReply
    | Links
    | EndOfLinks
    | EndOfNames
    | BanList
    | EndOfBanlist
    | EndOfWhoWas
    | Info
    | Motd
    | EndOfInfo
    | MotdStart
    | EndOfMotd
    | YoureOper
    | Rehashing
    | Time
    | UserSstart
    | Users
    | EndOfUsers
    | NoUsers
    | OtherReply Int
    deriving Show

reply :: Int -> Reply
reply 200 = TraceLink
reply 201 = TraceConnecting
reply 202 = TraceHandshake
reply 203 = TraceUnknown
reply 204 = TraceOperator
reply 205 = TraceUser
reply 206 = TraceServer
reply 208 = TraceNewtype
reply 211 = StatsLinkInfo
reply 212 = StatsCommands
reply 213 = StatsCLine
reply 214 = StatsNLine
reply 215 = StatsILine
reply 216 = StatsKLine
reply 218 = StatsYLine
reply 219 = EndOfStats
reply 221 = UModeIs
reply 241 = StatsLLine
reply 242 = StatsUpTime
reply 243 = StatsOLine
reply 244 = StatsHLine
reply 250 = StatsConn
reply 251 = LUserClient
reply 252 = LUserOp
reply 253 = LUserUnknown
reply 254 = LUserChannels
reply 255 = LUserMe
reply 256 = AdminMe
reply 257 = AdminLoc1
reply 258 = AdminLoc2
reply 259 = AdminEmail
reply 261 = TraceLog
reply 265 = LocalUsers
reply 266 = GlobalUsers
reply 300 = None
reply 301 = Away
reply 302 = UserHost
reply 303 = IsOn
reply 305 = Unaway
reply 306 = Nowaway
reply 311 = WhoIsUser
reply 312 = WhoIsServer
reply 313 = WhoIsOperator
reply 314 = WhoWasUser
reply 315 = EndOfWho
reply 317 = WhoIsIdle
reply 318 = EndOfWhois
reply 319 = WhoIsChannels
reply 321 = ListStart
reply 322 = List
reply 323 = ListEnd
reply 324 = ChannelModeIs
reply 331 = NoTopic
reply 332 = Topic
reply 341 = Inviting
reply 342 = Summoning
reply 351 = Version
reply 352 = WhoReply
reply 353 = NamReply
reply 364 = Links
reply 365 = EndOfLinks
reply 366 = EndOfNames
reply 367 = BanList
reply 368 = EndOfBanlist
reply 369 = EndOfWhoWas
reply 371 = Info
reply 372 = Motd
reply 374 = EndOfInfo
reply 375 = MotdStart
reply 376 = EndOfMotd
reply 381 = YoureOper
reply 382 = Rehashing
reply 391 = Time
reply 392 = UserSstart
reply 393 = Users
reply 394 = EndOfUsers
reply 395 = NoUsers
reply n   = OtherReply n

data Error =
      NoSuchNick
    | NoSuchServer
    | NoSuchChannel
    | CannotSendToChan
    | TooManyChannels
    | WasNoSuchNick
    | TooManyTargets
    | NoOrigin
    | NoRecipient
    | NoTextToSend
    | NoTopLevel
    | WildTopLevel
    | UnknownCommand
    | NoMotd
    | NoAdminInfo
    | FileError
    | NoNicknameGiven
    | ErrorneousNickname
    | NicknameInUse
    | NickCollision
    | UserNotInChannel
    | NotOnChannel
    | UserOnChannel
    | NoLogin
    | SummonDisabled
    | UsersDisabled
    | NotRegistered
    | NeedMoreParams
    | AlreadyRegistred
    | NopermForHost
    | PasswdMismatch
    | YoureBannedCreep
    | Keyset
    | ChannelIsFull
    | UnknownMode
    | InviteOnlyChan
    | BannedFromChan
    | BadChannelKey
    | NoPrivileges
    | ChanOprivsNeeded
    | CantKillServer
    | NoOperHost
    | UModeUnknownFlag
    | UsersDontMatch
    deriving Show

error_ :: Int -> Maybe Error
error_ 401 = Just NoSuchNick
error_ 402 = Just NoSuchServer
error_ 403 = Just NoSuchChannel
error_ 404 = Just CannotSendToChan
error_ 405 = Just TooManyChannels
error_ 406 = Just WasNoSuchNick
error_ 407 = Just TooManyTargets
error_ 409 = Just NoOrigin
error_ 411 = Just NoRecipient
error_ 412 = Just NoTextToSend
error_ 413 = Just NoTopLevel
error_ 414 = Just WildTopLevel
error_ 421 = Just UnknownCommand
error_ 422 = Just NoMotd
error_ 423 = Just NoAdminInfo
error_ 424 = Just FileError
error_ 431 = Just NoNicknameGiven
error_ 432 = Just ErrorneousNickname
error_ 433 = Just NicknameInUse
error_ 436 = Just NickCollision
error_ 441 = Just UserNotInChannel
error_ 442 = Just NotOnChannel
error_ 443 = Just UserOnChannel
error_ 444 = Just NoLogin
error_ 445 = Just SummonDisabled
error_ 446 = Just UsersDisabled
error_ 451 = Just NotRegistered
error_ 461 = Just NeedMoreParams
error_ 462 = Just AlreadyRegistred
error_ 463 = Just NopermForHost
error_ 464 = Just PasswdMismatch
error_ 465 = Just YoureBannedCreep
error_ 467 = Just Keyset
error_ 471 = Just ChannelIsFull
error_ 472 = Just UnknownMode
error_ 473 = Just InviteOnlyChan
error_ 474 = Just BannedFromChan
error_ 475 = Just BadChannelKey
error_ 481 = Just NoPrivileges
error_ 482 = Just ChanOprivsNeeded
error_ 483 = Just CantKillServer
error_ 491 = Just NoOperHost
error_ 501 = Just UModeUnknownFlag
error_ 502 = Just UsersDontMatch
error_ _   = Nothing

