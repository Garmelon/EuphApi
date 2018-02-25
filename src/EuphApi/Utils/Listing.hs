-- | This module helps with maintaining a list of connected clients.
--
-- It is supposed to be imported qualified under a different alias than
-- the other EuphApi modules, for example:
--
-- > import qualified EuphApi               as E
-- > import qualified EuphApi.Utils.Listing as EL

module EuphApi.Utils.Listing
  ( Listing
  , empty
  , toList
  , fromList
  -- * Update listing
  , update
  , add
  , remove
  , changeNick
  ) where

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent.STM
import qualified Data.Map               as M

import qualified EuphApi.Bot            as E
import qualified EuphApi.Types          as E

-- | Represents a listing (see <http://api.euphoria.io/#nick-event>)
-- in an easy-to-update way.
--
-- Usually, a bot does not track itself through this listing.
-- To include the bot, it must explicitly be 'add'ed and nick changes
-- must be tracked using 'changeNick'.
newtype Listing = Listing (M.Map E.SessionID E.SessionView)

-- | An empty listing.
empty :: Listing
empty = Listing M.empty

-- | Convert a listing to a list of 'E.SessionView's.
toList :: Listing -> [E.SessionView]
toList (Listing m) = map snd $ M.toList m

-- | Convert a list of 'E.SessionView's to a listing.
fromList :: [E.SessionView] -> Listing
fromList = Listing . M.fromList . map (\s -> (E.sessSessionID s, s))

-- TODO: Add some error checking and run 'E.who' when own Listing and server
-- response don't match.

-- | Updates a listing inside a 'TVar' according to the 'E.Event' given.
--
-- This function should be called inside your bot's event handler function.
-- The 'TVar' containing the listing should be a part of the connection specific
-- bot data.
update :: TVar Listing -> E.Event -> E.Bot b c ()
update lVar (E.SnapshotEvent _ list _ _) =
  liftIO $ atomically $ writeTVar lVar (fromList list)
update lVar (E.JoinEvent s) =
  withAskWho lVar $ liftIO $ atomically $ do
    (Listing m) <- readTVar lVar
    modifyTVar lVar (add s)
    return $ M.member (E.sessSessionID s) m
update lVar (E.PartEvent s) =
  withAskWho lVar $ liftIO $ atomically $ do
    (Listing m) <- readTVar lVar
    modifyTVar lVar (remove $ E.sessSessionID s)
    return $ not $ M.member (E.sessSessionID s) m
update lVar (E.NickEvent sid _ to) =
  withAskWho lVar $ liftIO $ atomically $ do
    (Listing m) <- readTVar lVar
    modifyTVar lVar (changeNick sid to)
    return $ not $ M.member sid m
update _ _ = return ()

withAskWho :: TVar Listing -> E.Bot b c Bool -> E.Bot b c ()
withAskWho lVar f = do
  ask <- f
  when ask $ E.fork $ E.who >>= (liftIO . atomically . writeTVar lVar . fromList)

-- | Add a new 'E.SessionView' to the listing (call on a 'E.JoinEvent').
add :: E.SessionView -> Listing -> Listing
add s (Listing m) = Listing $ M.insert (E.sessSessionID s) s m

-- | Remove a 'E.SessionView' from the listing (call on a 'E.PartEvent').
remove :: E.SessionID -> Listing -> Listing
remove sid (Listing m) = Listing $ M.delete sid m

-- | Set a new nick for a specific 'E.SessionView' (call on a 'E.NickEvent').
changeNick :: E.SessionID -> E.Nick -> Listing -> Listing
changeNick sid to (Listing m) = Listing $ M.adjust (\s -> s{E.sessName=to}) sid m
