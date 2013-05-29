{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Bunrui.Transcode (transcode) where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Arrow    ((&&&))
import Control.Monad    (filterM, forM_, join, unless, when, void)
import Data.Function    (on)
import Data.List        (stripPrefix)
import Data.Maybe       (fromMaybe)
import System.Directory (copyFile, createDirectory, doesDirectoryExist,
                         doesFileExist, getModificationTime)
import System.FilePath  ((</>), replaceExtension, takeExtension,
                         takeFileName)
import System.IO        (Handle, IOMode(WriteMode), withFile)
import System.Process
import Text.Printf      (printf)

import Bunrui.Core
import Bunrui.Util


type Extension = String
data Strategy = Transcode | Copy


isNewerThan :: FilePath -> FilePath -> IO Bool
isNewerThan = liftA2 (>) `on` getModificationTime

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
strategyForFile = fromMaybe (error "unknown extension") .
                  flip lookup extensions .
                  takeExtension

encodedExtension :: FilePath -> FilePath
encodedExtension src = go strategy src
    where go Transcode = flip replaceExtension ".m4a"
          go Copy      = id
          strategy     = strategyForFile src

pipeSource :: String -> [String] -> IO (Handle, ProcessHandle)
pipeSource name args = do
  (_, Just hout, _, p) <- createProcess $
                          (proc name args) {std_out = CreatePipe}
  return (hout, p)

decodeOgg :: FilePath -> IO (Handle, ProcessHandle)
decodeOgg path = pipeSource "oggdec" ["--quiet", "--output", "-", "--", path]

decodeFlac :: FilePath -> IO (Handle, ProcessHandle)
decodeFlac path = pipeSource "flac" ["--decode", "--stdout", "--silent",
                                     "--warnings-as-errors", path]

encodeM4A :: FilePath -> Metadata -> Handle -> ProcessHandle -> IO ()
encodeM4A dest metadata inputStream inputProcess =
    withFile "/dev/null" WriteMode $ \nul -> do
      { (_, _, _, h) <- createProcess $ (proc "faac" faacArgs) {
                          std_err = UseHandle nul
                        , std_in  = UseHandle inputStream
                        }
      ; void $ waitForProcess h >> waitForProcess inputProcess
      }
    where faacArgs = metadataArgs ++ ["-o", dest, "-q", "150", "-w", "-"]
          metadataArgs = concatMap (uncurry doArg)
                         [ ("--title",  return        . metaTitle)
                         , ("--artist", return        . metaArtist)
                         , ("--album",  return        . metaAlbum)
                         , ("--track",  return . show . metaTrack)
                         , ("--year",       fmap show . metaYear)
                         , ("--disc",       fmap show . formatDisc)
                         , ("--genre",                  metaGenre)
                         ]
          doArg :: String -> (Metadata -> Maybe String) -> [String]
          doArg name meta = maybe [] ((name:) . return) (meta metadata)
          formatDisc :: Metadata -> Maybe String
          formatDisc (Metadata {
                      metaDisc = disc
                    , metaTotalDiscs = totalDiscs
                    }) = printf "%d/%d" <$> disc <*> totalDiscs

doTranscode :: FilePath -> FilePath -> IO ()
doTranscode src dest = go (strategyForFile src) (takeExtension src)
    where go :: Strategy -> Extension -> IO ()
          go Transcode ".ogg"  = join $ encode <*> decodeOgg src
          go Transcode ".flac" = join $ encode <*> decodeFlac src
          go Transcode _       = error "unhandled extension"
          go Copy      _       = copyFile src dest
          encode = uncurry <$> encodeM4A dest <$> readMetadata src

rewritePath :: FilePath -> FilePath -> FilePath -> FilePath
rewritePath masters encoded = encodedExtension . (encoded </>) .
                              fromMaybe (error "not in " ++ masters) .
                              stripPrefix masters

transcode :: Command
transcode (Opts { mastersDirectory = masters
                , encodedDirectory = encoded
                , assumeYes = yes
                }) = do
  hasMasters <- doesDirectoryExist masters
  unless hasMasters $ error ("no such directory " ++ show masters)
  transcodes <- map (id &&& rewritePath masters encoded) <$>
                findSourceFiles masters
  stale <- filterM (uncurry isStale) transcodes
  unless (null stale) $ do
    continue <- orM [ return yes
                    , do forM_ stale (uncurry $ printf "  %s -> %s\n")
                         prompt
                    ]
    when continue $ do
      let width = maximum $ map (length . takeFileName . snd) stale
          format = "[%d/%d] Encoding %-" ++ show width ++ "s ( %s, %s )\n"
          total = length stale
      missing <- missingDirectories (map snd stale)
      mapM_ createDirectory missing
      parForIO_ (zip [1..] stale) $ \(n, (src, dest)) -> do
          printf format (n::Int) total (takeFileName dest) src dest
          doTranscode src dest
