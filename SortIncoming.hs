module Main where

import Control.Applicative ((<$>), (<*), (<*>))
import Control.Monad       (filterM, forM_, unless, when)
import Data.Char           (toLower)
import Data.Function       (on)
import Data.List           (delete, inits, intercalate, nub)
import Data.Text.Encoding  (encodeUtf8, decodeUtf8)
import Text.Printf         (printf)
import System.FilePath     (dropFileName, joinPath, splitDirectories,
                            takeExtension)
import System.Directory    (createDirectory, doesDirectoryExist,
                            doesFileExist, renameFile)
import System.Environment  (getArgs, getProgName)
import System.Exit         (ExitCode(..), exitFailure)
import System.FilePath     ((</>), combine)
import System.IO           (hPutStrLn, hPutStr, hFlush, stdout,
                            stderr)
import System.Process      (readProcessWithExitCode)
import System.Console.GetOpt
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

import System.FilePath.Find (always, extension, find)
import Text.Parsec          (Parsec, runParser, many,
                             many1, getState, modifyState, noneOf,
                             oneOf, char, newline, sepEndBy1)
import qualified Data.Text as T

data Opts = Opts
    { assumeYes         :: Bool
    , incomingDirectory :: FilePath
    , mastersDirectory  :: FilePath
    }

data Metadata = Metadata
    { metaExtension     :: String
    , metaAlbum         :: String
    , metaTitle         :: String
    , metaArtist        :: String
    , metaTrack         :: Integer
    }

type StringMap = M.Map String String

options :: [OptDescr (Opts -> Opts)]
options =
    [ Option "i" [] (ReqArg (\x o -> o {incomingDirectory = x}) "DIR")
                 "incoming directory"
    , Option "o" [] (ReqArg (\x o -> o {mastersDirectory = x})  "DIR")
                 "masters directory"
    , Option "y" [] (NoArg (\o -> o {assumeYes = True})) "assume yes"
    ]

optionDefaults :: Opts
optionDefaults =
    Opts { assumeYes = False
         , incomingDirectory = "Incoming"
         , mastersDirectory  = "Masters"
         }

usage :: IO a
usage = do
  progName <- getProgName
  hPutStrLn stderr $ usageInfo progName options
  exitFailure

parseOpts :: [String] -> IO Opts
parseOpts argv =
    case getOpt RequireOrder options argv of
      (opts, _, [])  -> return $ foldl (flip id) optionDefaults opts
      (_,    _, e:_) -> hPutStr stderr e >> usage

fromUTF8 :: String -> String
fromUTF8 = T.unpack . decodeUtf8 . C.pack

toUTF8 :: String -> String
toUTF8 = C.unpack . encodeUtf8 . T.pack

findFiles :: FilePath -> IO [FilePath]
findFiles root = map fromUTF8 <$> find always isSourceExtension root
    where isSourceExtension = flip elem sourceExtensions <$> extension
          sourceExtensions = [".ogg", ".flac"]

renameFile' :: FilePath -> FilePath -> IO ()
renameFile' = renameFile `on` toUTF8

createDirectory' :: FilePath -> IO ()
createDirectory' = createDirectory . toUTF8

doesFileExist' :: FilePath -> IO Bool
doesFileExist' = doesFileExist . toUTF8

doesDirectoryExist' :: FilePath -> IO Bool
doesDirectoryExist' = doesDirectoryExist . toUTF8

(!) :: Ord k => M.Map k a -> k -> Either k a
a ! k = maybe (Left k) Right (M.lookup k a)

metadata :: String
         -> StringMap
         -> Either String Metadata
metadata ext m = do
  album <- m ! "album"
  title <- m ! "title"
  artist <- m ! "artist"
  track <- read <$> m ! "tracknumber"
  when (track < 1) $ Left "non-positive track"
  return Metadata
             { metaExtension = ext
             , metaAlbum = album
             , metaTitle = title
             , metaArtist = artist
             , metaTrack = track
             }

commentParser :: Parsec String StringMap StringMap
commentParser = many (lineParser `sepEndBy1` newline) >> getState
    where lineParser = M.insert <$> key <* char '=' <*> value >>=
                       modifyState
          key = map toLower <$> many1 (oneOf $ delete '=' [' '..'}'])
          value = many (noneOf "\n\r")

runCommand :: String -> [String] -> IO String
runCommand cmd args = do
  res <- readProcessWithExitCode cmd (map toUTF8 args) ""
  case res of
    (ExitSuccess, out, _)   -> return out
    (_,           _,   err) -> error err

addMetadata :: FilePath -> IO Metadata
addMetadata path = either metadataError id .
                   either (error . show) (metadata ext) .
                   runParser commentParser M.empty path <$>
                   getMeta ext
    where ext = takeExtension path
          getMeta ".flac" = runCommand "metaflac"
                            ["--export-tags-to=-", path]
          getMeta ".ogg"  = runCommand "vorbiscomment" ["-e", path]
          getMeta _       = error $ "unhandled extension " ++ ext
          metadataError e = error $ toUTF8 path ++ ": " ++ e

getOutputPath :: Integer -> Metadata -> FilePath
getOutputPath lastTrack meta =
    foldr1 combine $ map (replace '/' '_') parts
    where replace x y = map (\a -> if a == x then y else a)
          parts = [metaArtist meta, metaAlbum meta, fileName]
          fileName = printf ("%." ++ lastTrackLength ++ "d %s%s")
                     (metaTrack meta) (metaTitle meta)
                     (metaExtension meta)
          lastTrackLength = show $ min 2 $ length $ show lastTrack

extractDirs :: FilePath -> [FilePath]
extractDirs = drop 1 . map joinPath . inits . splitDirectories .
              dropFileName

prompt :: [FilePath] -> [(FilePath, FilePath)] -> IO Bool
prompt newDirs txcodes = do
  putStrLn "Creating..."
  forM_ newDirs $ \ d -> putStrLn $ " * " ++ d
  putStrLn "Moving..."
  forM_ txcodes $ \(x, y) -> putStrLn $ " * " ++ x ++ " -> " ++ y
  putStr "Continue [y/N]? "
  hFlush stdout
  flip elem ["Y", "y"] <$> getLine

main :: IO ()
main = do
  args <- getArgs
  Opts { assumeYes = yes
       , incomingDirectory = incoming
       , mastersDirectory  = masters
       } <- parseOpts args
  sourceFiles <- findFiles incoming
  annotated <- mapM addMetadata sourceFiles
  let lastTrack = maximum (map metaTrack annotated)
      destFiles = map ((masters </>) . getOutputPath lastTrack) annotated
      destDirs = nub $ concatMap extractDirs destFiles
      toMove = zip sourceFiles destFiles
  wouldOverwrite <- filterM doesFileExist' destFiles
  unless (null wouldOverwrite) $
       error ("refusing to overwrite target files (would overwrite " ++
              intercalate ", " (map toUTF8 wouldOverwrite) ++ ")")
  missing <- filterM (fmap not . doesDirectoryExist') destDirs
  continue <- if yes then return True else prompt missing toMove
  when continue $ do
         mapM_ createDirectory' missing
         mapM_ (uncurry renameFile') toMove
