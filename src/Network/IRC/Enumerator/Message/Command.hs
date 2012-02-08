module Network.IRC.Enumerator.Message.Command (
    Command (..), Error (..), Reply (..)
  , knownCommand, knownReply, knownError
) where

import qualified Data.IntMap as IM
import qualified Data.Map    as M

import Data.ByteString.Char8 ( ByteString, pack, unpack )
import Data.Text ( Text )


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
    | C Text

    deriving (Eq, Ord, Show)

knownCommand :: ByteString -> Maybe Command
knownCommand = (`M.lookup` commandMap)

commandMap :: M.Map ByteString Command
commandMap = M.fromList $ map (\c -> ( pack (show c), c ))
    [ ADMIN
    , AWAY
    , CONNECT
    , DIE
    , ERROR
    , INFO
    , INVITE
    , ISON
    , JOIN
    , KICK
    , KILL
    , LINKS
    , LIST
    , LUSERS
    , MODE
    , MOTD
    , NAMES
    , NICK
    , NOTICE
    , OPER
    , PART
    , PASS
    , PING
    , PONG
    , PRIVMSG
    , QUIT
    , REHASH
    , RESTART
    , SERVICE
    , SERVLIST
    , SERVER
    , SQUERY
    , SQUIT
    , STATS
    , SUMMON
    , TIME
    , TOPIC
    , TRACE
    , USER
    , USERHOST
    , USERS
    , VERSION
    , WALLOPS
    , WHO
    , WHOIS
    , WHOWAS
    ]

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

    deriving (Eq, Ord, Show)

knownReply :: Int -> Maybe Reply
knownReply = (`IM.lookup` replyMap)

replyMap :: IM.IntMap Reply
replyMap = IM.fromList
    [ ( 200, TraceLink       ) 
    , ( 201, TraceConnecting ) 
    , ( 202, TraceHandshake  ) 
    , ( 203, TraceUnknown    ) 
    , ( 204, TraceOperator   ) 
    , ( 205, TraceUser       ) 
    , ( 206, TraceServer     ) 
    , ( 208, TraceNewtype    ) 
    , ( 211, StatsLinkInfo   ) 
    , ( 212, StatsCommands   ) 
    , ( 213, StatsCLine      ) 
    , ( 214, StatsNLine      ) 
    , ( 215, StatsILine      ) 
    , ( 216, StatsKLine      ) 
    , ( 218, StatsYLine      ) 
    , ( 219, EndOfStats      ) 
    , ( 221, UModeIs         ) 
    , ( 241, StatsLLine      ) 
    , ( 242, StatsUpTime     ) 
    , ( 243, StatsOLine      ) 
    , ( 244, StatsHLine      ) 
    , ( 250, StatsConn       ) 
    , ( 251, LUserClient     ) 
    , ( 252, LUserOp         ) 
    , ( 253, LUserUnknown    ) 
    , ( 254, LUserChannels   ) 
    , ( 255, LUserMe         ) 
    , ( 256, AdminMe         ) 
    , ( 257, AdminLoc1       ) 
    , ( 258, AdminLoc2       ) 
    , ( 259, AdminEmail      ) 
    , ( 261, TraceLog        ) 
    , ( 265, LocalUsers      ) 
    , ( 266, GlobalUsers     ) 
    , ( 300, None            ) 
    , ( 301, Away            ) 
    , ( 302, UserHost        ) 
    , ( 303, IsOn            ) 
    , ( 305, Unaway          ) 
    , ( 306, Nowaway         ) 
    , ( 311, WhoIsUser       ) 
    , ( 312, WhoIsServer     ) 
    , ( 313, WhoIsOperator   ) 
    , ( 314, WhoWasUser      ) 
    , ( 315, EndOfWho        ) 
    , ( 317, WhoIsIdle       ) 
    , ( 318, EndOfWhois      ) 
    , ( 319, WhoIsChannels   ) 
    , ( 321, ListStart       ) 
    , ( 322, List            ) 
    , ( 323, ListEnd         ) 
    , ( 324, ChannelModeIs   ) 
    , ( 331, NoTopic         ) 
    , ( 332, Topic           ) 
    , ( 341, Inviting        ) 
    , ( 342, Summoning       ) 
    , ( 351, Version         ) 
    , ( 352, WhoReply        ) 
    , ( 353, NamReply        ) 
    , ( 364, Links           ) 
    , ( 365, EndOfLinks      ) 
    , ( 366, EndOfNames      ) 
    , ( 367, BanList         ) 
    , ( 368, EndOfBanlist    ) 
    , ( 369, EndOfWhoWas     ) 
    , ( 371, Info            ) 
    , ( 372, Motd            ) 
    , ( 374, EndOfInfo       ) 
    , ( 375, MotdStart       ) 
    , ( 376, EndOfMotd       ) 
    , ( 381, YoureOper       ) 
    , ( 382, Rehashing       ) 
    , ( 391, Time            ) 
    , ( 392, UserSstart      ) 
    , ( 393, Users           ) 
    , ( 394, EndOfUsers      ) 
    , ( 395, NoUsers         ) 
    ]

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

    deriving (Eq, Ord, Show)

knownError :: Int -> Maybe Error
knownError = (`IM.lookup` errorMap)

errorMap :: IM.IntMap Error
errorMap = IM.fromList
    [ ( 401, NoSuchNick         ) 
    , ( 402, NoSuchServer       ) 
    , ( 403, NoSuchChannel      ) 
    , ( 404, CannotSendToChan   ) 
    , ( 405, TooManyChannels    ) 
    , ( 406, WasNoSuchNick      ) 
    , ( 407, TooManyTargets     ) 
    , ( 409, NoOrigin           ) 
    , ( 411, NoRecipient        ) 
    , ( 412, NoTextToSend       ) 
    , ( 413, NoTopLevel         ) 
    , ( 414, WildTopLevel       ) 
    , ( 421, UnknownCommand     ) 
    , ( 422, NoMotd             ) 
    , ( 423, NoAdminInfo        ) 
    , ( 424, FileError          ) 
    , ( 431, NoNicknameGiven    ) 
    , ( 432, ErrorneousNickname ) 
    , ( 433, NicknameInUse      ) 
    , ( 436, NickCollision      ) 
    , ( 441, UserNotInChannel   ) 
    , ( 442, NotOnChannel       ) 
    , ( 443, UserOnChannel      ) 
    , ( 444, NoLogin            ) 
    , ( 445, SummonDisabled     ) 
    , ( 446, UsersDisabled      ) 
    , ( 451, NotRegistered      ) 
    , ( 461, NeedMoreParams     ) 
    , ( 462, AlreadyRegistred   ) 
    , ( 463, NopermForHost      ) 
    , ( 464, PasswdMismatch     ) 
    , ( 465, YoureBannedCreep   ) 
    , ( 467, Keyset             ) 
    , ( 471, ChannelIsFull      ) 
    , ( 472, UnknownMode        ) 
    , ( 473, InviteOnlyChan     ) 
    , ( 474, BannedFromChan     ) 
    , ( 475, BadChannelKey      ) 
    , ( 481, NoPrivileges       ) 
    , ( 482, ChanOprivsNeeded   ) 
    , ( 483, CantKillServer     ) 
    , ( 491, NoOperHost         ) 
    , ( 501, UModeUnknownFlag   ) 
    , ( 502, UsersDontMatch     ) 
    ]
