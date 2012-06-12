{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Bunrui.Transcode (transcode) where

import Control.Applicative ((<$>))
import Control.Arrow    ((&&&))
import Control.Monad    (filterM, forM_, liftM2, void, unless, when)
import Data.Function    (on)
import Data.List        (nub, stripPrefix)
import Data.Maybe       (fromJust)
import System.Directory (copyFile, createDirectory, doesDirectoryExist,
                         doesFileExist, getModificationTime)
import System.FilePath  ((</>), replaceExtension, takeExtension,
                         takeFileName)
import System.IO        (Handle, IOMode(WriteMode), withFile)
import System.Process
import Text.Printf      (printf)

import Control.Concurrent.Spawn (parMapIO_, pool)
import GHC.Conc (numCapabilities)

import Bunrui.Core
import Bunrui.Util


type Extension = String
data Strategy = Transcode | Copy


compareModificationTime :: FilePath -> FilePath -> IO Ordering
compareModificationTime = liftM2 compare `on` getModificationTime

isNewerThan :: FilePath -> FilePath -> IO Bool
x `isNewerThan` y = (GT ==) <$> x `compareModificationTime` y

isStale :: FilePath -> FilePath -> IO Bool
isStale x y = orM [ not <$> doesFileExist y
                  , x `isNewerThan` y
                  ]

extensions :: [(Extension, Strategy)]
extensions = [ (".ogg", Transcode)
             , (".flac", Transcode)
             , (".m4a", Copy)
             , (".mp3", Copy)
             ]

strategyForFile :: FilePath -> Strategy
strategyForFile = maybe (error "unknown extension") id .
                  flip lookup extensions .
                  takeExtension

encodedExtension :: FilePath -> FilePath
encodedExtension src = go strategy src
    where go Transcode = flip replaceExtension ".m4a"
          go Copy      = id
          strategy     = strategyForFile src

decodeOgg :: FilePath -> IO (Metadata, Handle)
decodeOgg path = liftM2 (,) metadata handle
    where metadata = readMetadata path
          handle = do
            let oggdecArgs = ["--quiet", "--output", "-", "--", path]
                process = (proc "oggdec" oggdecArgs) {
                            std_out = CreatePipe}
            (_, Just hout, _, _) <- createProcess process
            return hout

decodeFlac :: FilePath -> IO (Metadata, Handle)
decodeFlac = undefined

encodeM4A :: FilePath -> Metadata -> Handle -> IO ()
encodeM4A dest metadata inputStream =
  withFile "/dev/null" WriteMode
               $ \nul -> do
                 (_, _, _, h) <- createProcess $ (proc "faac" faacArgs) {
                                        std_err = UseHandle nul
                                      , std_in  = UseHandle inputStream
                                      }
                 void $ waitForProcess h
    where faacArgs = metadataArgs ++ ["-o", dest, "-q", "150", "-w", "-"]
          metadataArgs = concatMap (uncurry doArg) $
                         [ ("--title",  return . metaTitle)
                         , ("--artist", return . metaArtist)
                         , ("--album",  return . metaAlbum)
                         , ("--track",  return . show . metaTrack)
                         , ("--year",   fmap show . metaYear)
                         , ("--genre",  metaGenre)
                         ]
          doArg :: String -> (Metadata -> Maybe String) -> [String]
          doArg name meta = maybe [] ((name:) . return) (meta metadata)

doTranscode :: FilePath -> FilePath -> IO ()
doTranscode src dest = go strategy ext
    where go :: Strategy -> Extension -> IO ()
          go Transcode ".ogg"  = decodeOgg src >>= encode
          go Transcode ".flac" = decodeFlac src >>= encode
          go Transcode _       = error "unhandled extension"
          go Copy      _       = copyFile src dest
          strategy = strategyForFile src
          ext      = takeExtension src
          encode   = uncurry (encodeM4A dest)


shouldContinue :: [FilePath] -> [(FilePath, FilePath)] -> IO ()
shouldContinue missing stale = do
  unless (null missing) $ do
    putStrLn "New directories:"
    mapM_ putStrLn $ map ("  " ++) missing
  putStrLn "Transcodes:"
  forM_ stale $ \(x, y) -> printf "  %s -> %s\n" x y

transcode :: Command
transcode (Opts { mastersDirectory = masters
                , encodedDirectory = encoded
                , assumeYes = yes
                }) = do
  let toDestPath = encodedExtension . (encoded </>) .
                   fromJust . stripPrefix masters
  transcodes <- map (id &&& toDestPath) <$> findSourceFiles masters
  stale <- filterM (uncurry isStale) transcodes
  when (null stale) $ error "nothing to do"
  missing <- filterM (fmap not . doesDirectoryExist) $ nub $
             concatMap (leadingPathComponents . snd) stale
  continue <- if yes then return True
              else shouldContinue missing stale >> prompt
  when continue $ do
    let width = maximum $ map (length . takeFileName . snd) stale
        format = "[%d/%d] Encoding %-" ++ show width ++ "s ( %s, %s )\n"
        total = length stale
    mapM_ createDirectory missing
    p <- pool numCapabilities
    flip parMapIO_ (zip [1..] stale) $ \(n, (src, dest)) -> p $ do
        printf format (n::Int) total (takeFileName dest) src dest
        doTranscode src dest
