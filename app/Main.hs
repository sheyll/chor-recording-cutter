{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Text.Read
import System.FilePath
import System.Process
import Data.List
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Printf
import System.Environment
import Data.Char
import System.Posix.Files (fileExist)
import Data.Monoid (mempty)
import Data.String
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes ()
import qualified Text.Blaze.Html5.Attributes as A


data Src =
  Src { srcFile  :: FilePath
      , srcStart :: Seconds
      , srcEnd   :: Seconds
      }
      deriving (Eq, Ord)

instance Show Src where
  showsPrec d (Src s a o) =
    showParen (d > 10)
       ( showString s
         . showString " | "
         . showsPrec (d+1) a
         . showString " | "
         . showsPrec (d+1) o
       )

newtype Seconds = Seconds Integer
  deriving (Eq, Ord)

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

data Meta =
  Meta { artist :: String
       , album  :: String
       , title  :: String
       }
  deriving (Eq, Ord, Show, Read)

data Codec = Wav | Aac | Mp3
      deriving (Bounded, Enum, Eq, Ord, Show, Read)

codecFileExtension :: Codec -> FilePath
codecFileExtension Aac = "m4a"
codecFileExtension Mp3 = "mp3"
codecFileExtension Wav = "wav"

ffmpegCodec :: Codec -> Maybe String
ffmpegCodec Aac = Just "aac"
ffmpegCodec Mp3 = Just "mp3"
ffmpegCodec Wav = Nothing

data Voice = Bass | Tenor | Alt | Sopran | Teacher
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Composition = Chor | Unisono | Solo | Speak
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Instrument = Vocals | Piano | Violine | Flute | Percussion
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Performance = Performance Intend [Voice] [Composition] [Instrument]
  deriving (Read, Show, Eq, Ord)

data Intend = Concert | Rehearsal | Training
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

toMetaDataComment :: Cut -> String
toMetaDataComment (Cut (Track _ (Performance intend vs cs instruments) _) (Src srcFile startTime stopTime)) =
  intercalate "; "
  [case intend of
     Concert -> "Concert recording"
     Rehearsal -> "Complete rehearsal"
     Training -> "Training session"
  ,"Source file: " ++ takeFileName srcFile ++ " (" ++ show startTime ++ " - " ++ show stopTime ++ ")"
  ,if null vs then "" else "Voices: " ++ intercalate ", " (show <$> vs)
  ,if null cs then "" else "Voice compositions: " ++ intercalate ", " (show <$> cs)
  ,if null instruments then "" else "Instruments: " ++ intercalate ", " (show <$> instruments)
  ]

hasIntend :: Intend -> Performance -> Bool
hasIntend v (Performance w _ _ _) = v == w

hasVoiceGroup :: [Voice] -> Performance -> Bool
hasVoiceGroup vs (Performance _ ws _ _) = sort vs == sort ws

getVoices :: Performance -> [Voice]
getVoices (Performance _ vs _ _) = vs

hasNoVoices :: Performance -> Bool
hasNoVoices (Performance _ [] _ _) = True
hasNoVoices _ = False

data Song = Song Meta [Clip]
  deriving ( Eq, Ord, Show, Read)

readSongs :: FilePath -> IO [Song]
readSongs =  readFile >=> (return . read)

foldSong :: Monoid a
         => (Meta -> Performance -> Src -> Integer -> Integer -> a)
         -> Song
         -> a
foldSong f (Song meta clips) =
  mconcat $
  do (clipIndex, (fullTitle, Clip performance sources))
       <- zip [1..] $
         do clip0 <- clips
            let mkClips c@(Clip _ _) t    = [(t, c)]
                mkClips (Titled t2 cs) t1 =
                    [tc | ct <- cs, tc <- mkClips ct (t1 ++ ", " ++ t2)]
            mkClips clip0 (title meta)
     (srcIndex, src) <- zip [1..] sources
     return (f (meta {title = fullTitle}) performance src clipIndex srcIndex)

data Clip = Clip Performance [Src]
          | Titled String [Clip]
  deriving ( Eq, Ord, Read, Show)

data FfmpegArgs = FfmpegArgs { ffmpegOutFile :: FilePath
                             , fromFfmpegArgs :: [String] }
              deriving Show

data Track = Track { trackMeta :: Meta
                   , trackContent :: Performance
                   , trackIndex :: Integer
                   }
  deriving ( Eq, Ord, Read, Show)

trackFilename :: Track -> FilePath
trackFilename (Track meta (Performance intend voices compositions instruments) i) =
   title meta
   <.> show intend
   <.> printf "%0.6d" i
   <.> intercalate "_" (show <$> (sort voices))
   <.> intercalate "_" (show <$> (sort compositions))
   <.> intercalate "_" (show <$> (sort instruments))
   <.> artist meta

data Cut = Cut { outputTrack :: Track
               , inputSource :: Src
               }
  deriving ( Eq, Ord, Read, Show)

cutOutputFilename :: Cut -> FilePath
cutOutputFilename c =
  trackFilename (outputTrack c)

cutSongBySource :: Song -> [Cut]
cutSongBySource = foldSong $ \m p s ci si ->
  let maxSources = 100
      in [Cut (Track m p ((ci - 1) * 100 + rem si 100)) s]

ffmpegCutCommand :: FilePath -> FilePath -> Codec -> Cut -> FfmpegArgs
ffmpegCutCommand inRoot outRoot codec cut =
         let outFile =
               outRoot </> cutOutputFilename cut <.> codecFileExtension codec
             prArg :: String -> String
             prArg =  printf "%s"
             prArgEq :: String -> String -> String
             prArgEq l r = printf "%s=%s" l r
             prMeta l f =  prArgEq l (f (trackMeta (outputTrack cut)))
             Src inFile t0 t = inputSource cut
             outCodecParams = maybe [] (\fc -> ["-codec:a", fc, "-b:a", "256k"]) (ffmpegCodec codec)
             outIndex = trackIndex (outputTrack cut)
             in FfmpegArgs outFile
                           ([ "-i", prArg (inRoot </> inFile)
                            , "-ss", prArg (show t0)
                            , "-to", prArg (show t)
                            ] ++ outCodecParams ++
                            [ "-metadata", prMeta "artist" artist
                            , "-metadata", prMeta "album" album
                            , "-metadata", prMeta "title" title
                            , "-metadata", prArgEq "track" (show outIndex)
                            , "-metadata", prArgEq "comment" (toMetaDataComment cut)
                            , prArg outFile
                            , "-y"])


cutAndTranscodeSongsBySource :: FilePath -> FilePath -> Codec -> [Song] -> [(Cut, FfmpegArgs)]
cutAndTranscodeSongsBySource inFileRootDir outFileRootDir codec =
  foldMap transcodeSong
  where
    transcodeSong :: Song -> [(Cut, FfmpegArgs)]
    transcodeSong song =
      do cut <- cutSongBySource song
         let cmd = ffmpegCutCommand inFileRootDir outFileRootDir codec cut
         return (cut, cmd)

songToHtml :: FilePath -> Codec -> Song -> String
songToHtml outDir codec song =
  renderHtml
   (songToHtmlPage song
     (do let cuts = cutSongBySource song
         forM_
           [minBound .. maxBound]
           (\intend ->
              do let tracksWithIntend = filter (hasIntend intend . trackContent . outputTrack) cuts
                 unless (null tracksWithIntend)
                   (do H.h2 (fromString (case intend of
                                           Concert -> "Concert Recordings"
                                           Rehearsal -> "Complete Rehearsals"
                                           Training -> "Incomplete Training Tracks"))
                       sequence_ (trackToHtml outDir codec . outputTrack <$> tracksWithIntend)))))


songHtmlFilename :: Song -> FilePath
songHtmlFilename (Song meta _) =
  toLower <$> filter isAlphaNum (title meta) <.> "html"

songToHtmlPage :: Song -> H.Html -> H.Html
songToHtmlPage (Song meta _) body =
   H.docTypeHtml $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
            H.title (fromString (title meta ++ " - " ++ album meta ++ " - " ++ artist meta))
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.media "screen" ! A.href "main.css"
            H.script ! A.src "main.js" $ mempty
        H.body $ do
            H.div ! A.class_ "overview-link" $ H.a ! A.href "index.html" $ "Recording Overview"
            H.h1 $ fromString $ title meta
            body
            H.p ! A.class_ "feedback" $ do
                H.img ! A.src "mail.gif"
                "Questions, ideas and complaints are all equally welcome. Drop a mail: "
                H.a ! A.href "mailto:svh@posteo.de" $ "svh@posteo.de"

trackToHtml :: FilePath -> Codec -> Track -> H.Html
trackToHtml outDir codec t = do
   let
     meta = trackMeta t
     Performance intend voices compositions instruments = trackContent t
     fname = outDir </> trackFilename t <.> codecFileExtension codec
     audioId = takeFileName fname

   H.h3 $ fromString (title meta  ++ " (" ++ show intend ++ ") - " ++ show (trackIndex t) )
   H.div ! A.class_ "player" $
     do H.audio
          ! A.id (fromString audioId)
          ! A.src (fromString fname)
          ! A.controls ""
          $ "Your Browser cannot play this audio. Please install the latest firefox browser."
        H.p ! A.class_ "download" $
         H.a ! H.customAttribute "download" (fromString audioId) ! A.href (fromString fname) $ "Download (AAC)"
        H.h4 "Track Contents"
        unless (null voices) $ H.div $ do
          H.strong "Voices:"
          fromString (intercalate ", " ( show <$> voices))
        unless (null instruments) $ H.div $ do
          H.strong "Instruments:"
          fromString (intercalate ", " ( show <$> instruments))
        unless (null compositions) $ H.div $ do
          H.strong "Arrangements:"
          fromString (intercalate ", " ( show <$> compositions))

ensureNonOverlappingOutputFilenames :: [Cut] -> IO ()
ensureNonOverlappingOutputFilenames cuts =
  let outFiles = trackFilename . outputTrack <$> cuts
      overlappingFiles = outFiles \\ nub outFiles
  in
    if null overlappingFiles then
      return ()
    else
      fail ("Duplicate output files: " ++ show overlappingFiles)

main :: IO ()
main = do
  args'''' <- getArgs
  let (simulate, args''') =
        case args'''' of
          ("N" : rest) -> (True, rest)
          rest         -> (False, rest)
      (htmlOutput, args'') =
        case args''' of
          ("--html" : rest) -> (True, rest)
          rest              -> (False, rest)
      (noAudioFiles, args') =
        case args'' of
          ("--skip-audio" : rest) -> (True, rest)
          rest              -> (False, rest)
      (overwriteAudio, args) =
        case args' of
          ("--overwrite-audio" : rest) -> (True, rest)
          rest              -> (False, rest)
      (clipsFile, inDir, outDir)
        = case args of
            [] -> ("clips", ".", ".")
            [f] -> (f, ".", ".")
            [f, i] -> (f, i, ".")
            (f: i: o: _) -> (f, i, o)
  let codec = Aac
  songs <- readSongs clipsFile
  let (cuts, ffmpegCommands) = unzip (cutAndTranscodeSongsBySource inDir outDir codec songs)
  ensureNonOverlappingOutputFilenames cuts
  if simulate
      then
        do mapM_ (putStrLn . show .  fromFfmpegArgs) ffmpegCommands
           when htmlOutput
             (mapM_ (putStrLn . renderHtml . trackToHtml outDir codec . outputTrack) cuts)
      else
        do unless noAudioFiles
             (mapM_ (\ffArgs ->
                       do exists <- fileExist (ffmpegOutFile ffArgs)
                          if not exists || overwriteAudio then
                            callProcess "/usr/bin/ffmpeg" (fromFfmpegArgs ffArgs)
                           else
                            putStrLn (ffmpegOutFile ffArgs ++ " already exists.")
                    )
               ffmpegCommands)
           when htmlOutput
             (mapM_ (\song ->
                       do putStrLn ("Generating "++songHtmlFilename song)
                          writeFile
                            (outDir </> songHtmlFilename song)
                            (songToHtml outDir codec song)) songs)
