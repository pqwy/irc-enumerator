{-# LANGUAGE OverloadedStrings  #-}

module Network.HIRC.Message.Interp (
    Command (..), interp, Error (..), Reply (..)
) where

import Data.Monoid
import Data.Maybe
import Data.ByteString.Char8 ( ByteString, readInt )

data Command = Normal ByteString
             | Reply Reply
             | Error Error
    deriving Show

interp :: ByteString -> Command
interp cmd =
    case readInt cmd of
         Just (n, "") ->
             fromMaybe norm
             ( pickleft [ Reply `fmap` asReply n
                        , Error `fmap` asError n ] )
         _ -> norm
  where
    norm = Normal cmd

pickleft :: [Maybe a] -> Maybe a
pickleft = getFirst . mconcat . map First

data Reply = TraceLink
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

    deriving Show

asReply :: Int -> Maybe Reply
asReply 200 = Just TraceLink
asReply 201 = Just TraceConnecting
asReply 202 = Just TraceHandshake
asReply 203 = Just TraceUnknown
asReply 204 = Just TraceOperator
asReply 205 = Just TraceUser
asReply 206 = Just TraceServer
asReply 208 = Just TraceNewtype
asReply 211 = Just StatsLinkInfo
asReply 212 = Just StatsCommands
asReply 213 = Just StatsCLine
asReply 214 = Just StatsNLine
asReply 215 = Just StatsILine
asReply 216 = Just StatsKLine
asReply 218 = Just StatsYLine
asReply 219 = Just EndOfStats
asReply 221 = Just UModeIs
asReply 241 = Just StatsLLine
asReply 242 = Just StatsUpTime
asReply 243 = Just StatsOLine
asReply 244 = Just StatsHLine
asReply 250 = Just StatsConn
asReply 251 = Just LUserClient
asReply 252 = Just LUserOp
asReply 253 = Just LUserUnknown
asReply 254 = Just LUserChannels
asReply 255 = Just LUserMe
asReply 256 = Just AdminMe
asReply 257 = Just AdminLoc1
asReply 258 = Just AdminLoc2
asReply 259 = Just AdminEmail
asReply 261 = Just TraceLog
asReply 265 = Just LocalUsers
asReply 266 = Just GlobalUsers
asReply 300 = Just None
asReply 301 = Just Away
asReply 302 = Just UserHost
asReply 303 = Just IsOn
asReply 305 = Just Unaway
asReply 306 = Just Nowaway
asReply 311 = Just WhoIsUser
asReply 312 = Just WhoIsServer
asReply 313 = Just WhoIsOperator
asReply 314 = Just WhoWasUser
asReply 315 = Just EndOfWho
asReply 317 = Just WhoIsIdle
asReply 318 = Just EndOfWhois
asReply 319 = Just WhoIsChannels
asReply 321 = Just ListStart
asReply 322 = Just List
asReply 323 = Just ListEnd
asReply 324 = Just ChannelModeIs
asReply 331 = Just NoTopic
asReply 332 = Just Topic
asReply 341 = Just Inviting
asReply 342 = Just Summoning
asReply 351 = Just Version
asReply 352 = Just WhoReply
asReply 353 = Just NamReply
asReply 364 = Just Links
asReply 365 = Just EndOfLinks
asReply 366 = Just EndOfNames
asReply 367 = Just BanList
asReply 368 = Just EndOfBanlist
asReply 369 = Just EndOfWhoWas
asReply 371 = Just Info
asReply 372 = Just Motd
asReply 374 = Just EndOfInfo
asReply 375 = Just MotdStart
asReply 376 = Just EndOfMotd
asReply 381 = Just YoureOper
asReply 382 = Just Rehashing
asReply 391 = Just Time
asReply 392 = Just UserSstart
asReply 393 = Just Users
asReply 394 = Just EndOfUsers
asReply 395 = Just NoUsers
asReply _     = Nothing


data Error = NoSuchNick
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

asError :: Int -> Maybe Error
asError 401 = Just NoSuchNick
asError 402 = Just NoSuchServer
asError 403 = Just NoSuchChannel
asError 404 = Just CannotSendToChan
asError 405 = Just TooManyChannels
asError 406 = Just WasNoSuchNick
asError 407 = Just TooManyTargets
asError 409 = Just NoOrigin
asError 411 = Just NoRecipient
asError 412 = Just NoTextToSend
asError 413 = Just NoTopLevel
asError 414 = Just WildTopLevel
asError 421 = Just UnknownCommand
asError 422 = Just NoMotd
asError 423 = Just NoAdminInfo
asError 424 = Just FileError
asError 431 = Just NoNicknameGiven
asError 432 = Just ErrorneousNickname
asError 433 = Just NicknameInUse
asError 436 = Just NickCollision
asError 441 = Just UserNotInChannel
asError 442 = Just NotOnChannel
asError 443 = Just UserOnChannel
asError 444 = Just NoLogin
asError 445 = Just SummonDisabled
asError 446 = Just UsersDisabled
asError 451 = Just NotRegistered
asError 461 = Just NeedMoreParams
asError 462 = Just AlreadyRegistred
asError 463 = Just NopermForHost
asError 464 = Just PasswdMismatch
asError 465 = Just YoureBannedCreep
asError 467 = Just Keyset
asError 471 = Just ChannelIsFull
asError 472 = Just UnknownMode
asError 473 = Just InviteOnlyChan
asError 474 = Just BannedFromChan
asError 475 = Just BadChannelKey
asError 481 = Just NoPrivileges
asError 482 = Just ChanOprivsNeeded
asError 483 = Just CantKillServer
asError 491 = Just NoOperHost
asError 501 = Just UModeUnknownFlag
asError 502 = Just UsersDontMatch
asError _     = Nothing


