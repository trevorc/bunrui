module Bunrui.Incoming (sortIncoming) where

import Control.Monad       (filterM, forM_, unless, when)
import Data.List           (intercalate, nub)
import Text.Printf         (printf)
import System.FilePath     ((</>), combine)
import System.Directory    (createDirectory, doesDirectoryExist,
                            doesFileExist, renameFile)

import Bunrui.Core
import Bunrui.Util


masterPath :: Integer -> Metadata -> FilePath
masterPath lastTrack meta =
    foldr1 combine $ map (replace '/' '_') parts
    where parts = [metaArtist meta, metaAlbum meta, fileName]
          fileName = printf ("%." ++ lastTrackLength ++ "d %s%s")
                     (metaTrack meta) (metaTitle meta)
                     (metaExtension meta)
          lastTrackLength = show $ min 2 $ length $ show lastTrack

shouldContinue :: [FilePath] -> [(FilePath, FilePath)] -> IO ()
shouldContinue newDirs txcodes = do
  putStrLn "Creating"
  putStrLn "========"
  forM_ newDirs $ \ d -> putStrLn $ " * " ++ d
  putStrLn "\nMoving"
  putStrLn "======"
  forM_ txcodes $ \(x, y) -> putStrLn $ " * " ++ x ++ " -> " ++ y

sortIncoming :: Command
sortIncoming (Opts { assumeYes = yes
                   , incomingDirectory = incoming
                   , mastersDirectory  = masters
                   }) = do
  doesDirectoryExist incoming `unlessM`
       error ("no such directory " ++ incoming)
  sourceFiles <- findSourceFiles incoming
  when (null sourceFiles) $ error "nothing to do"
  annotated <- mapM readMetadata sourceFiles
  let lastTrack = maximum (map metaTrack annotated)
      destFiles = map ((masters </>) . masterPath lastTrack) annotated
      destDirs = nub $ concatMap leadingPathComponents destFiles
      toMove = zip sourceFiles destFiles
  wouldOverwrite <- filterM doesFileExist destFiles
  unless (null wouldOverwrite) $
       error ("refusing to overwrite target files (would overwrite " ++
              intercalate ", " wouldOverwrite ++ ")")
  missing <- filterM (fmap not . doesDirectoryExist) destDirs
  continue <- if yes then return True
              else shouldContinue missing toMove >> prompt
  when continue $ do
    mapM_ createDirectory missing
    mapM_ (uncurry renameFile) toMove
