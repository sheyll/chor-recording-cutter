module Song
where

import           Control.Monad
import           Text.Read
import           System.FilePath
import           System.Process
import           Data.List
import           Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec
                                               as RP
import           Text.Printf
import           System.Environment
import           Data.Char
import           System.Posix.Files             ( fileExist )
import           Data.Monoid                    ( mempty )
import           Data.String

data Song = Song Meta [Clip]
  deriving ( Eq, Ord, Show, Read)

data Meta =
  Meta { artist :: String
       , album  :: String
       , title  :: String
       }
  deriving (Eq, Ord, Show, Read)

data Clip = Clip Performance [Src]
          | Titled String [Clip]
  deriving ( Eq, Ord, Read, Show)

data Performance = Performance Intend [Voice] [Composition] [Instrument]
  -- TODO change to: Performance Intend [Instrument] Accompaniment
  deriving (Read, Show, Eq, Ord)

newtype Accompaniment = Accompaniment [Instrument]

data Intend =
    Concert -- ^ deprecated use 'PublicPerformance'
    | PublicPerformance
    | Rehearsal
    | TeacherDirections
    | Pronunciation
    | Training
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Instrument =
    Vocals -- TODO add [Voice] [Composition]
    | Piano
    | Violine -- ^ deprecated use 'Violin' instead
    | Violin
    | Flute
    | Percussion
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Voice = Bass | Tenor | Alt | Sopran | Teacher
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Composition =
    Chor -- ^ deprecated use 'Choir' instead
    | Choir
    | Unisono
    | Solo
    | Speak  -- ^ deprecated use the intend 'Pronunciation'
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Src =
  Src { srcFile  :: FilePath
      , srcStart :: Seconds
      , srcEnd   :: Seconds
      }
      deriving (Eq, Ord)

newtype Seconds = Seconds Integer
  deriving (Eq, Ord)

-- unsorted:

instance Show Src where
  showsPrec d (Src s a o) =
    showParen (d > 10)
       ( showString s
         . showString " | "
         . showsPrec (d+1) a
         . showString " | "
         . showsPrec (d+1) o
       )

instance Show Seconds where
  showsPrec _d (Seconds t0) =
        let segs0 =
              unfoldr (\t ->
                          if t > 0 then
                            Just ( t `rem` 60, t `Prelude.div` 60)
                          else
                            Nothing)
              t0
            segs = if null segs0 then [0] else segs0
        in showString (intercalate ":" (foldr (\x acc -> acc ++ [printf "%0.2d" x]) [] segs))

instance Read Seconds where
  readsPrec _d = readP_to_S $
    do
       ess <- (readS_to_P (readsPrec 0) :: ReadP Integer)  `sepBy` char ':'
       return (Seconds (fst (foldr (\n (acc, fac) -> (acc + fac * n, fac * 60)) (0, 1) ess)))


instance Read Src where
  readPrec =
    parens $ RP.readP_to_Prec $ const $
      do skipSpaces
         s' <- Text.ParserCombinators.ReadP.get `manyTill` (char '|')
         let s = reverse (dropWhile isSpace (reverse s'))
         skipSpaces
         b <- readS_to_P (readsPrec 0)
         skipSpaces
         void (char '|')
         skipSpaces
         e <- readS_to_P (readsPrec 0)
         skipSpaces
         return (Src s b e)
