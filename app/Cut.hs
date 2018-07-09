module Cut
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
import           Song

data Codec = Aac | Mp3 | Wav
    deriving (Read, Show, Ord, Enum, Eq)

codecFileExtension :: Codec -> FilePath
codecFileExtension Aac = "m4a"
codecFileExtension Mp3 = "mp3"
codecFileExtension Wav = "wav"

ffmpegCodec :: Codec -> Maybe String
ffmpegCodec Aac = Just "aac"
ffmpegCodec Mp3 = Just "mp3"
ffmpegCodec Wav = Nothing

data Cut = Cut { outputTrack :: Track
               , inputSource :: Src
               }
  deriving ( Eq, Ord, Read, Show)

toMetaDataComment :: Cut -> String
toMetaDataComment (Cut (Track _ (Performance intend vs cs instruments) _) (Src srcFile startTime stopTime))
    = intercalate
        "; "
        [ case intend of
            Concert           -> "Concert recording"
            Rehearsal         -> "Complete rehearsal"
            TeacherDirections -> "Teacher Directions"
            Pronunciation     -> "Pronunciation Training"
            Training          -> "Training session"
        , "Source file: "
        ++ takeFileName srcFile
        ++ " ("
        ++ show startTime
        ++ " - "
        ++ show stopTime
        ++ ")"
        , if null vs then "" else "Voices: " ++ intercalate ", " (show <$> vs)
        , if null cs
            then ""
            else "Voice compositions: " ++ intercalate ", " (show <$> cs)
        , if null instruments
            then ""
            else "Instruments: " ++ intercalate ", " (show <$> instruments)
        ]

hasIntend :: Intend -> Performance -> Bool
hasIntend v (Performance w _ _ _) = v == w

hasVoiceGroup :: [Voice] -> Performance -> Bool
hasVoiceGroup vs (Performance _ ws _ _) = sort vs == sort ws

getVoices :: Performance -> [Voice]
getVoices (Performance _ vs _ _) = vs

hasNoVoices :: Performance -> Bool
hasNoVoices (Performance _ [] _ _) = True
hasNoVoices _                      = False

foldSong
    :: Monoid a
    => (Meta -> Performance -> Src -> Integer -> Integer -> a)
    -> Song
    -> a
foldSong f (Song meta clips) = mconcat $ do
    (clipIndex, (fullTitle, Clip performance sources)) <- zip [1 ..] $ do
        clip0 <- clips
        let mkClips c@(Clip _ _) t = [(t, c)]
            mkClips (Titled t2 cs) t1 =
                [ tc | ct <- cs, tc <- mkClips ct (t1 ++ ", " ++ t2) ]
        mkClips clip0 (title meta)
    (srcIndex, src) <- zip [1 ..] sources
    return (f (meta { title = fullTitle }) performance src clipIndex srcIndex)

data FfmpegArgs = FfmpegArgs { ffmpegOutFile :: FilePath
                             , fromFfmpegArgs :: [String] }
              deriving Show

data Track = Track { trackMeta :: Meta
                   , trackContent :: Performance
                   , trackIndex :: Integer
                   }
  deriving ( Eq, Ord, Read, Show)

trackFilename :: Track -> FilePath
trackFilename (Track meta (Performance intend voices compositions instruments) i)
    = title meta
        <.> show intend
        <.> printf "%0.6d" i
        <.> intercalate "_" (show <$> (sort voices))
        <.> intercalate "_" (show <$> (sort compositions))
        <.> intercalate "_" (show <$> (sort instruments))
        <.> artist meta

cutOutputFilename :: Cut -> FilePath
cutOutputFilename c = trackFilename (outputTrack c)

cutSongBySource :: Song -> [Cut]
cutSongBySource = foldSong $ \m p s ci si ->
    let maxSources = 100 in [Cut (Track m p ((ci - 1) * 100 + rem si 100)) s]

ffmpegCutCommand :: FilePath -> FilePath -> Codec -> Cut -> FfmpegArgs
ffmpegCutCommand inRoot outRoot codec cut
    = let
          outFile =
              outRoot </> cutOutputFilename cut <.> codecFileExtension codec
          prArg :: String -> String
          prArg = printf "%s"
          prArgEq :: String -> String -> String
          prArgEq l r = printf "%s=%s" l r
          prMeta l f = prArgEq l (f (trackMeta (outputTrack cut)))
          Src inFile t0 t = inputSource cut
          outCodecParams  = maybe []
                                  (\fc -> ["-codec:a", fc, "-b:a", "256k"])
                                  (ffmpegCodec codec)
          outIndex = trackIndex (outputTrack cut)
      in
          FfmpegArgs
              outFile
              (  [ "-i"
                 , prArg (inRoot </> inFile)
                 , "-ss"
                 , prArg (show t0)
                 , "-to"
                 , prArg (show t)
                 ]
              ++ outCodecParams
              ++ [ "-metadata"
                 , prMeta "artist" artist
                 , "-metadata"
                 , prMeta "album" album
                 , "-metadata"
                 , prMeta "title" title
                 , "-metadata"
                 , prArgEq "track" (show outIndex)
                 , "-metadata"
                 , prArgEq "comment" (toMetaDataComment cut)
                 , prArg outFile
                 , "-y"
                 ]
              )


cutAndTranscodeSongsBySource
    :: FilePath -> FilePath -> Codec -> [Song] -> [(Cut, FfmpegArgs)]
cutAndTranscodeSongsBySource inFileRootDir outFileRootDir codec = foldMap
    transcodeSong
  where
    transcodeSong :: Song -> [(Cut, FfmpegArgs)]
    transcodeSong song = do
        cut <- cutSongBySource song
        let cmd = ffmpegCutCommand inFileRootDir outFileRootDir codec cut
        return (cut, cmd)

