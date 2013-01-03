module Bunrui.Incoming (sortIncoming) where

import Control.Monad       (filterM, forM_, unless, when)
import Data.List           (intercalate)
import Text.Printf         (printf)
import System.FilePath     ((</>), combine)
import System.Directory    (createDirectory, doesDirectoryExist,
                            doesFileExist, renameFile)

import Data.Text               (pack, unpack)
import Data.Text.ICU.Normalize (NormalizationMode, normalize)

import Bunrui.Core
import Bunrui.Util


masterPath :: Integer -> NormalizationMode -> Metadata -> FilePath
masterPath lastTrack mode meta =
    foldr1 combine $ map (normalize' . replace '/' '_') parts
    where parts = [metaArtist meta, metaAlbum meta, fileName]
          normalize' = unpack . normalize mode . pack
          fileName = printf ("%." ++ lastTrackLength ++ "d %s%s")
                     (metaTrack meta) (metaTitle meta)
                     (metaExtension meta)
          lastTrackLength = show $ min 2 $ length $ show lastTrack

sortIncoming :: Command
sortIncoming (Opts { assumeYes         = yes
                   , incomingDirectory = incoming
                   , mastersDirectory  = masters
                   , normalizationMode = mode
                   }) = do
  hasIncoming <- doesDirectoryExist incoming
  unless hasIncoming $ error $ "no such directory " ++ incoming
  sourceFiles <- findSourceFiles incoming
  unless (null sourceFiles) $ do
    annotated <- mapM readMetadata sourceFiles
    let lastTrack = maximum (map metaTrack annotated)
        destFiles = map ((masters </>) . masterPath lastTrack mode) annotated
        toMove = zip sourceFiles destFiles
    wouldOverwrite <- filterM doesFileExist destFiles
    unless (null wouldOverwrite) $
           error ("refusing to overwrite target files (would overwrite " ++
                  intercalate ", " wouldOverwrite ++ ")")
    missing <- missingDirectories destFiles
    continue <- orM [ return yes
                    , forM_ toMove (uncurry $ printf "  %s -> %s\n") >> prompt
                    ]
    when continue $ do
      mapM_ createDirectory missing
      mapM_ (uncurry renameFile) toMove
