{-# LANGUAGE OverloadedStrings #-}
module Main
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
import           Text.Blaze.Html.Renderer.Pretty
                                                ( renderHtml )
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes    ( )
import qualified Text.Blaze.Html5.Attributes   as A
import           Song
import           Cut


readSongs :: FilePath -> IO [Song]
readSongs = readFile >=> (return . read)


songToHtml :: FilePath -> Codec -> Song -> String
songToHtml outDir codec song = renderHtml
  (songToHtmlPage
    song
    (do
      let cuts = cutSongBySource song
      forM_
        [minBound .. maxBound]
        (\intend -> do
          let tracksWithIntend =
                filter (hasIntend intend . trackContent . outputTrack) cuts
          unless
            (null tracksWithIntend)
            (do
              H.h2
                (fromString
                  (case intend of
                    Concert           -> "Concert Recordings"
                    Rehearsal         -> "Complete Rehearsals"
                    TeacherDirections -> "Teacher Commentary"
                    Pronunciation     -> "Pronunciation Training"
                    Training          -> "Training Tracks"
                  )
                )
              mapM_
                (\cut -> do
                  trackToHtml outDir codec (outputTrack cut)
                )
                tracksWithIntend
            )
        )
    )
  )


songHtmlFilename :: Song -> FilePath
songHtmlFilename (Song meta _) =
  toLower <$> filter isAlphaNum (title meta) <.> "html"

songToHtmlPage :: Song -> H.Html -> H.Html
songToHtmlPage (Song meta _) body = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
    H.title
      (fromString (title meta ++ " - " ++ album meta ++ " - " ++ artist meta))
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.media "screen" ! A.href
      "main.css"
    H.script ! A.src "main.js" $ mempty
  H.body $ do
    H.div
      ! A.class_ "overview-link"
      $ H.a
      ! A.href "index.html"
      $ "Recording Overview"
    H.h1 $ fromString $ title meta
    body
    H.p ! A.class_ "feedback" $ do
      H.img ! A.src "mail.gif"
      "Questions, ideas and complaints are all equally welcome. Drop a mail: "
      H.a ! A.href "mailto:svh@posteo.de" $ "svh@posteo.de"

trackToHtml :: FilePath -> Codec -> Track -> H.Html
trackToHtml outDir codec t = do
  let meta    = trackMeta t
      Performance intend voices compositions instruments = trackContent t
      fname   = outDir </> trackFilename t <.> codecFileExtension codec
      audioId = takeFileName fname

  if (null voices)
    then H.h3 $ fromString
      (title meta ++ " (" ++ show intend ++ ") - " ++ show (trackIndex t))
    else H.h3 $ do
      fromString $ intercalate ", " $ show <$> voices
      when (null instruments) $ H.i "A Cappella"
  H.div ! A.class_ "player" $ do
    H.audio
      ! A.id (fromString audioId)
      ! A.src (fromString fname)
      ! A.controls ""
      $ "Your Browser cannot play this audio."
    when (not (null voices)) $ H.h4 $ fromString
      (title meta ++ " (" ++ show intend ++ ") - " ++ show (trackIndex t))
    when (not (null instruments) && null voices) $ H.h4 "Instrumental"
    unless (null instruments) $ H.div $ do
      H.strong "Instruments:"
      fromString (intercalate ", " (show <$> instruments))
    unless (null compositions) $ H.div $ do
      H.strong "Arrangements:"
      fromString (intercalate ", " (show <$> compositions))
    H.p
      ! A.class_ "download"
      $ H.a
      ! H.customAttribute "download" (fromString audioId)
      ! A.href (fromString fname)
      $ "Download"

ensureNonOverlappingOutputFilenames :: [Cut] -> IO ()
ensureNonOverlappingOutputFilenames cuts =
  let outFiles         = trackFilename . outputTrack <$> cuts
      overlappingFiles = outFiles \\ nub outFiles
  in  if null overlappingFiles
        then return ()
        else fail ("Duplicate output files: " ++ show overlappingFiles)



main :: IO ()
main = do
  args'''' <- getArgs
  let (simulate, args''') = case args'''' of
        ("N" : rest) -> (True, rest)
        rest         -> (False, rest)
      (htmlOutput, args'') = case args''' of
        ("--html" : rest) -> (True, rest)
        rest              -> (False, rest)
      (noAudioFiles, args') = case args'' of
        ("--skip-audio" : rest) -> (True, rest)
        rest                    -> (False, rest)
      (overwriteAudio, args) = case args' of
        ("--overwrite-audio" : rest) -> (True, rest)
        rest                         -> (False, rest)
      (clipsFile, inDir, outDir) = case args of
        []              -> ("clips", ".", ".")
        [f]             -> (f, ".", ".")
        [f, i]          -> (f, i, ".")
        (f : i : o : _) -> (f, i, o)
  let codec = Aac
  songs <- readSongs clipsFile
  let (cuts, ffmpegCommands) =
        unzip (cutAndTranscodeSongsBySource inDir outDir codec songs)
  ensureNonOverlappingOutputFilenames cuts
  if simulate
    then do
      mapM_ (putStrLn . show . fromFfmpegArgs) ffmpegCommands
      when
        htmlOutput
        (mapM_
          (putStrLn . renderHtml . trackToHtml outDir codec . outputTrack)
          cuts
        )
    else do
      unless
        noAudioFiles
        (mapM_
          (\ffArgs -> do
            exists <- fileExist (ffmpegOutFile ffArgs)
            if not exists || overwriteAudio
              then callProcess "/usr/bin/ffmpeg" (fromFfmpegArgs ffArgs)
              else putStrLn (ffmpegOutFile ffArgs ++ " already exists.")
          )
          ffmpegCommands
        )
      when
        htmlOutput
        (mapM_
          (\song -> do
            putStrLn ("Generating " ++ songHtmlFilename song)
            writeFile (outDir </> songHtmlFilename song)
                      (songToHtml outDir codec song)
          )
          songs
        )
