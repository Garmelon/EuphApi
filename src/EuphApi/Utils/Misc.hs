{-# LANGUAGE OverloadedStrings #-}

module EuphApi.Utils.Misc (
  -- * Nick manipulation
    mention
  , atMention
  , mentionReduce
  , similar
  -- * Time manipulation
  , printUTCTime
  , printNominalDiffTime
  , printUptime
  ) where

import           Data.Char
import           Data.Function

import qualified Data.Text     as T
import           Data.Time

{-
 - Nick manipulation
 -}

-- | Convert a nick to an @-mentionable version.
-- Use this function when you want to @-mention somebody.
--
-- This removes spaces and some extra characters, while trying to stay close to
-- the original nick.
mention :: T.Text -> T.Text
mention = T.filter (\c -> not (isSpace c) && notElem c (".!?;&<'\"" :: String))

-- | Same as 'atMention', but prepends an `@` character.
atMention :: T.Text -> T.Text
atMention = T.cons '@' . mention

-- | Reduces a nick to a normal form such that all nicks that get @-mentioned
-- by the same @-mention are reduced to the same normal form.
--
-- Use this function when you want to compare two nicks.
mentionReduce :: T.Text -> T.Text
mentionReduce = T.map toLower . mention

-- | Compare two nicks using 'mentionReduce'.
similar :: T.Text -> T.Text -> Bool
similar = (==) `on` mentionReduce

{-
 - Time manipulation
 -}

-- | Convert a 'UTCTime' into a 'String' of the format:
-- @yyyy-mm-dd HH:MM:SS UTC@.
--
-- Example: @2018-02-13 20:43:33 UTC@
printUTCTime :: UTCTime -> String
printUTCTime = formatTime defaultTimeLocale "%F %T %Z"

-- | Convert a 'NominalDiffTime' into a 'String' of the format:
-- @[[[[w] d] h] m] s@ (weeks, days, hours, minutes, seconds)
--
-- Examples: @3h 12m 55s@ and @4w 6d 1h 0m 0s@
printNominalDiffTime :: NominalDiffTime -> String
printNominalDiffTime n =
  let nr = abs $ round n :: Integer
      (w, wr) = nr `quotRem` (60 * 60 * 24 * 7)
      (d, dr) = wr `quotRem` (60 * 60 * 24    )
      (h, hr) = dr `quotRem` (60 * 60         )
      (m, s ) = hr `quotRem`  60
      ws = if w /= 0 then show w ++ "w " else ""
      ds = if d /= 0 then show d ++ "d " else ""
      hs = if h /= 0 then show h ++ "h " else ""
      ms = if m /= 0 then show m ++ "m " else ""
      ss =                show s ++ "s"
      sign = if n < 0 then "-" else ""
  in  sign ++ ws ++ ds ++ hs ++ ms ++ ss

-- | @printUptime start now@ converts the two times @start@ and @now@
-- into a string of the following format, according to the botrulez:
--
-- @/me has been up since \<start\> (\<now - start\>).@
--
-- Example: @/me has been up since 2018-02-13 20:43:33 UTC (3h 12m 55s).@
printUptime :: UTCTime -> UTCTime -> String
printUptime start now =
  let diff    = diffUTCTime now start
      upSince = printUTCTime start
      upFor   = printNominalDiffTime diff
  in  "/me has been up since " ++ upSince ++ " (" ++ upFor ++ ")."
