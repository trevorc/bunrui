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
import Data.Traversable    (for)
import System.Directory    (doesDirectoryExist)
import System.FilePath     (dropFileName, joinPath, splitDirectories,
                            takeExtension)
import qualified Data.Map as M

import Data.Text.ICU.Normalize (NormalizationMode)
import System.FilePath.Find    (always, extension, find)
import Text.Parsec             (Parsec, runParser, many,
                                many1, getState, modifyState, noneOf,
                                oneOf, char, newline, sepEndBy1)

import Bunrui.Util


data Opts = Opts
    { assumeYes         :: Bool
    , incomingDirectory :: FilePath
    , mastersDirectory  :: FilePath
    , encodedDirectory  :: FilePath
    , normalizationMode :: NormalizationMode
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

parse :: Read r => String -> String -> Either String r
parse e x = maybeToEither errMsg $ maybeRead x
    where errMsg = "couldn't parse " ++ e ++ ": " ++ x

metadata :: String
         -> M.Map String String
         -> Either String Metadata
metadata ext m = do
  album <- m ! "album"
  title <- m ! "title"
  artist <- m ! "artist"
  trackNumber <- parse "tracknumber" =<< m ! "tracknumber"
  when (trackNumber < 1) $ Left "non-positive track"
  year <- for (M.lookup "year" m) $ parse "year"
  let genre = M.lookup "genre" m
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
