module Bunrui.Incoming (sortIncoming) where

import Control.Arrow       (first)
import Control.Monad       (filterM, forM_, unless, when)
import Data.List           (intercalate, partition)
import Data.Maybe          (catMaybes, fromJust, isJust)
import Text.Printf         (printf)
import System.FilePath     ((</>), combine)
import System.Directory    (createDirectory, doesDirectoryExist,
                            doesFileExist, renameFile)

import Data.Text               (pack, unpack)
import Data.Text.ICU.Normalize (NormalizationMode, normalize)

import Bunrui.Core
import Bunrui.Util


masterPath :: Integer           -- | highest track number
           -> (Maybe Integer)   -- | Just highest disc number or Nothing
           -> NormalizationMode
           -> Metadata
           -> FilePath
masterPath lastTrack lastDisc mode meta =
    foldr1 combine $ map (normalize' . replace '/' '_') parts
    where parts = catMaybes [ Just (metaAlbumArtist meta)
                            , Just (metaAlbum meta)
                            , lastDisc >>= discName
                            , Just fileName
                            ]
          normalize' = unpack . normalize mode . pack
          discName lastDiscNo = if lastDiscNo > 1
                                  then Just $ printf ("CD %." ++ intLength 1 lastDiscNo ++ "d")
                                           (fromJust $ metaDisc meta)
                                  else Nothing
          fileName = printf ("%." ++ intLength 2 lastTrack ++ "d %s%s")
                     (metaTrack meta) (metaTitle meta)
                     (metaExtension meta)
          intLength minLength = show . min minLength . length . show

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
    let (withDisc, withoutDisc)
            = first catMaybes .
              partition isJust .
              map metaDisc $ annotated
        lastTrack = maximum (map metaTrack annotated)
        lastDisc = case withDisc of
                     [] -> Nothing
                     _  -> Just (maximum withDisc)
        destFiles = map ((masters </>) .
                         masterPath lastTrack lastDisc mode)
                    annotated
        toMove = zip sourceFiles destFiles
    when (null withDisc == null withoutDisc) $
         error "some files missing a disc number"
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
