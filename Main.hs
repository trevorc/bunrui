module Main where

import Control.Category   ((>>>))
import Control.Monad      (mzero)
import Data.List          (intercalate)
import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure)
import System.FilePath    (addTrailingPathSeparator)
import System.IO          (hPutStrLn, stderr)
import System.Console.GetOpt

import Data.Text.ICU.Normalize (NormalizationMode(NFC, NFD))

import Bunrui.Core
import Bunrui.Incoming
import Bunrui.Transcode
import Bunrui.Util


instance Read NormalizationMode where
    readsPrec _ r = do
      (m, s) <- lex r
      case m of
        "NFC" -> return (NFC, s)
        "NFD" -> return (NFD, s)
        _     -> mzero

commands :: [(String, Command)]
commands = [("incoming", sortIncoming),
            ("transcode", transcode)]

options :: [OptDescr (Opts -> Opts)]
options =
    [ Option "i" [] (ReqArg (addTrailingPathSeparator >>> \x o ->
                                 o {incomingDirectory = x}) "DIR")
                 "incoming directory"
    , Option "m" [] (ReqArg (addTrailingPathSeparator >>> \x o ->
                                 o {mastersDirectory = x})  "DIR")
                 "masters directory"
    , Option "e" [] (ReqArg (addTrailingPathSeparator >>> \x o ->
                                 o {encodedDirectory = x})  "DIR")
                 "encoded directory"
    , Option "n" [] (ReqArg (read >>> \x o ->
                                 o {normalizationMode = x}) "MODE")
                 "normalization mode"
    , Option "y" [] (NoArg (\o -> o {assumeYes = True})) "assume yes"
    ]

optionDefaults :: Opts
optionDefaults =
    Opts { assumeYes = False
         , incomingDirectory = "Incoming/"
         , mastersDirectory  = "Masters/"
         , encodedDirectory  = "Encoded/"
         , normalizationMode = NFC
         }

usage :: String -> IO a
usage msg = do
  progName <- getProgName
  let puts = hPutStrLn stderr
      commandList = intercalate ",\n  " $ map fst commands
  puts msg
  puts $ usageInfo (progName ++ " command") options
  puts $ "where command is:\n  " ++ commandList
  exitFailure

parseOpts :: [String] -> Either String (Command, Opts)
parseOpts argv = do
  (opts, arg) <- case getOpt RequireOrder options argv of
                   (opts, [arg], [])  -> return (opts, arg)
                   (_,    _,     [])  -> Left "command required"
                   (_,    _,     e:_) -> Left e
  let errMsg = "invalid command " ++ arg
      opts' = foldl (flip id) optionDefaults opts
  cmd <- maybeToEither errMsg $ lookup arg commands
  return (cmd, opts')

main :: IO ()
main = getArgs >>=
       either usage return .
       parseOpts >>=
       uncurry id
