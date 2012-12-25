module Bunrui.Core
    ( Command
    , Metadata(..)
    , Opts(..)
    , findSourceFiles
    , missingDirectories
    , readMetadata
    ) where

import Control.Applicative ((<$>), (<*), (<*>))
import Control.Monad       (filterM, when)
import Data.Char           (toLower)
import Data.List           (delete, inits, nub)
import System.Directory    (doesDirectoryExist)
import System.FilePath     (dropFileName, joinPath, splitDirectories,
                            takeExtension)
import qualified Data.Map as M

import System.FilePath.Find (always, extension, find)
import Text.Parsec          (Parsec, runParser, many,
                             many1, getState, modifyState, noneOf,
                             oneOf, char, newline, sepEndBy1)

import Bunrui.Util


data Opts = Opts
    { assumeYes         :: Bool
    , incomingDirectory :: FilePath
    , mastersDirectory  :: FilePath
    , encodedDirectory  :: FilePath
    }

type Command = Opts -> IO ()

data Metadata = Metadata
    { metaExtension     :: String
    , metaAlbum         :: String
    , metaTitle         :: String
    , metaArtist        :: String
    , metaTrack         :: Integer
    , metaYear          :: Maybe Integer
    , metaGenre         :: Maybe String
    } deriving Show

type StringMap = M.Map String String


(!) :: Ord k => M.Map k a -> k -> Either k a
a ! k = maybeToEither k (M.lookup k a)

metadata :: String
         -> M.Map String String
         -> Either String Metadata
metadata ext m = do
  album <- m ! "album"
  title <- m ! "title"
  artist <- m ! "artist"
  track <- m ! "tracknumber"
  let genre = M.lookup "genre" m
      errMsg = "tracknumber did not parse: " ++ track
  trackNumber <- maybeToEither errMsg $ maybeRead track
  when (trackNumber < 1) $ Left "non-positive track"
  year <- case M.lookup "year" m of
            Nothing   -> return Nothing
            Just year -> Just <$> maybeToEither ("bad year: " ++ show year) (maybeRead year)
  return Metadata
             { metaExtension = ext
             , metaAlbum = album
             , metaTitle = title
             , metaArtist = artist
             , metaTrack = trackNumber
             , metaGenre = genre
             , metaYear = year
             }

commentParser :: Parsec String StringMap StringMap
commentParser = many (lineParser `sepEndBy1` newline) >> getState
    where lineParser = M.insert <$> key <* char '=' <*> value >>=
                       modifyState
          key = map toLower <$> many1 (oneOf $ delete '=' [' '..'}'])
          value = many (noneOf "\n\r")

readMetadata :: FilePath -> IO Metadata
readMetadata path = either metadataError id .
                    either (error . show) (metadata ext) .
                    runParser commentParser M.empty path <$>
                    getMeta ext
    where ext = takeExtension path
          getMeta ".flac" = runCommand "metaflac"
                            ["--export-tags-to=-", path]
          getMeta ".ogg"  = runCommand "vorbiscomment" ["-e", path]
          getMeta _       = error $ "unhandled extension " ++ ext
          metadataError e = error $ path ++ ": " ++ e

findSourceFiles :: FilePath -> IO [FilePath]
findSourceFiles = find always isSourceExtension
    where isSourceExtension = fmap (`elem` sourceExtensions) extension
          sourceExtensions = [".ogg", ".flac"]

leadingPathComponents :: FilePath -> [FilePath]
leadingPathComponents = drop 1 . map joinPath . inits .
                        splitDirectories . dropFileName

missingDirectories :: [FilePath] -> IO [FilePath]
missingDirectories = filterM (fmap not . doesDirectoryExist) .
                     nub . concatMap leadingPathComponents
