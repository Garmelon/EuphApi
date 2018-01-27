{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module implements a few types from the Euphoria API at
-- <http://api.euphoria.io/#overview>.

module EuphApi.Types
  ( Snowflake
  , SessionID
  , UserID(..)
  , UserType(..)
  , Message(..)
  , SessionView(..)
  ) where

import           Data.Aeson
import           Data.Function
import qualified Data.Text     as T
import           Data.Time

-- | Represents <http://api.euphoria.io/#snowflake>.
--
-- A 'Snowflake' is a 13-character string, usually used as a unique identifier
-- for some type of object.
-- It is the base-36 encoding of an unsigned, 64-bit integer.
type Snowflake = T.Text

-- | ID of a session, unique across all sessions globally.
type SessionID = T.Text

-- | Represents <http://api.euphoria.io/#userid>.
--
-- A 'UserID' identifies a user.
-- The type of session, 'UserType', can be retrieved via 'userType'.
data UserID = UserID
  { userType      :: UserType
  , userSnowflake :: Snowflake
  } deriving (Show, Eq)

instance FromJSON UserID where
  parseJSON = withText "UserID" $ \t ->
    let (tp, sf) = T.breakOn ":" t
        userType = findUserType tp
        userSnowflake = T.drop 1 sf
    in  return $ if userType == Other
                   then UserID{userSnowflake=t, ..}
                   else UserID{..}
    where
      findUserType txt
        | txt == "account" = Account
        | txt == "bot"     = Bot
        | txt == "agent"   = Agent
        | otherwise        = Other


-- | Whether a user is logged in, out, or a bot.
--
-- See <http://api.euphoria.io/#userid> for more info.
data UserType = Agent
              | Account
              | Bot
              | Other
  deriving (Show, Eq)

-- | Represents <http://api.euphoria.io/#message>.
--
-- A 'Message' is a node in a Room’s Log.
-- It corresponds to a chat message, or a post, or any broadcasted event in a room
-- that should appear in the log.
--
-- The fields @previous_edit_id@ and @encryption_key_id@ are not implemented.
data Message = Message
  { msgID        :: Snowflake
    -- ^ The id of the message (unique within a room)
  , msgParent    :: Maybe Snowflake
    -- ^ The id of the message's parent, or Nothing if top-level
  , msgTime      :: UTCTime
    -- ^ The unix timestamp of when the message was posted
  , msgSender    :: SessionView
    -- ^ The view of the sender's session
  , msgContent   :: String
    -- ^ The content of the message (client-defined)
  , msgEdited    :: Maybe UTCTime
    -- ^ The unix timestamp of when the message was last edited
  , msgDeleted   :: Maybe UTCTime
    -- ^ The unix timestamp of when the message was deleted
  , msgTruncated :: Bool
    -- ^ If true, then the full content of this message is not included.

  -- , msgPreviousEditId :: MaybeSnowflake -- not implemented
  -- , msgEncryptionKeyID :: String -- not implemented
  } deriving (Show)

instance Eq Message where
  (==) = (==) `on` msgID

instance Ord Message where
  compare = compare `on` msgID

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    msgID        <- o .: "id"
    msgParent    <- o .:? "parent"
    msgTime      <- o .: "time"
    msgSender    <- o .: "sender"
    msgContent   <- o .: "content"
    msgEdited    <- o .:? "edited"
    msgDeleted   <- o .:? "deleted"
    msgTruncated <- o .:? "truncated" .!= False
    return $ Message{..}

-- | Represents <http://api.euphoria.io/#sessionview>.
--
-- A 'SessionView' describes a session and its identity.
--
-- The fields @client_address@ and @real_client_address@ are not implemented.
data SessionView = SessionView
  { sessID        :: UserID
  , sessName      :: String
  , sessServerID  :: String
  , sessServerEra :: String
  , sessSessionID :: SessionID
  , isStaff       :: Bool
  , isManager     :: Bool
  -- , sessClientAddress :: String -- not implemented
  -- , sessRealClientAddress :: String -- not implemented
  } deriving (Show)

instance FromJSON SessionView where
  parseJSON = withObject "SessionView" $ \o -> do
    sessID        <- o .: "id"
    sessName      <- o .: "name"
    sessServerID  <- o .: "server_id"
    sessServerEra <- o .: "server_era"
    sessSessionID <- o .: "session_id"
    isStaff       <- o .:? "is_staff" .!= False
    isManager     <- o .:? "is_manager" .!= False
    return $ SessionView{..}
