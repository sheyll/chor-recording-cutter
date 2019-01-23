module SongParser
    ( parseSongs
    )
where

import           Control.Monad                  ( void )
import           Data.Void
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer    as L
import           Song

-- | Parse a markdown file into a song list:
--
-- Parse a markdown-like text document into a list of 'Song's.
--
-- Each song is contained in an album, which is a level-1 title, and begins
-- with a level-2 headline containing the song title.
--
-- A top heading is the album title:
-- >
-- > # Album title
-- >
-- Optionally followed by a year in parenthesis, e.g. @(2019)@
--
-- >
-- > # Rainbow Songs (2021)
-- >
--
-- The Album title is followed by newlines and then immediately by the __/artist/__
-- using markdown syntax for bold text:
--
-- >
-- > # Rainbow Songs (2021)
-- >
-- > **Singers For Peace**
-- >
--
-- A song is then started by the song title with a subtitle:
--
-- >
-- > # Rainbow Songs (2021)
-- >
-- > **Singers For Peace**
-- >
-- > ## Song of Friendship
-- >
--
-- A song consists of several clips. A clip has sources, main instruments (including vocals) and
-- zero or more accompaniment instruments.
--
-- The 'Intend' may be varied  at any level using
-- a heading or bold text at the corresponding level, containing a valid 'Intend' string.
-- The default intend is 'Training'.
--
-- The Accompaniment can also be specified by a heading or bold text prefixed with the
-- word @Accompaniment:@ (with capital @A@ and a colon). The scope of the definition is
-- accordingly. The accompaniment can be reset using @Accompaniment: none@
--
-- >
-- > # Rainbow Songs (2021)
-- >
-- > **Singers For Peace**
-- >
-- > ## Song of Friendship (Training)
-- >
-- > ### Bar 10 - 22
-- >
-- > - Vocals: Bass, Tenor, Alt, Sopran, Teacher  (Choir, Solo, Unisono)
-- > - Flute, Piano, Violin, Percussion
-- > 1. [ 0:05 - 01:23 ][1]
-- > 2. [ 0:25 - 00:59 ][1]
-- > 3. [ 1:03 - 01:39 ][3]
-- >
-- > - Vocals: Bass, Teacher  (Solo)
-- > 1. [ 2:25 - 03:59 ][1]
-- >
-- > #### Bar 12 - 14
-- >
-- > - Flute
-- > - Piano, Vocals: Alt, Sopran (Unisono), Violin
-- > 1. [ 2:25 - 03:59 ][1]
-- >
-- > ### Bar 10 - 22
-- >
-- > The above resets the level-4 title, also this text is ignored.
-- >
-- > - Vocals: Bass, Teacher  (Solo)
-- > - Piano
-- > 1. [ 2:25 - 03:59 ][1]



parseSongs :: Text -> Either String Song
parseSongs _ = Left "TODO"


