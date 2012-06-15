module Bunrui.Util where

import Prelude hiding (any, foldr)
import Control.Applicative ((<$>))
import Control.Monad  (foldM)
import Data.List      (inits)
import Control.Monad  (when, unless)
import System.Exit    (ExitCode(..))
import System.FilePath (dropFileName, joinPath, splitDirectories)
import System.Process (readProcessWithExitCode)
import System.IO      (hFlush, stdout)


maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing

replace :: (Functor f, Eq a) => a -> a -> f a -> f a
replace x y = fmap $ \a -> if a == x then y else a

runCommand :: String -> [String] -> IO String
runCommand cmd args = do
  res <- readProcessWithExitCode cmd args ""
  case res of
    (ExitSuccess, out, _)   -> return out
    (_,           _,   err) -> error err

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond = (cond >>=) . flip when

unlessM :: Monad m => m Bool-> m () -> m ()
unlessM cond = (cond >>=) . flip unless

orM :: Monad m => [m Bool] -> m Bool
orM = foldM (\a x -> if a then return a else x) False

leadingPathComponents :: FilePath -> [FilePath]
leadingPathComponents = drop 1 . map joinPath . inits .
                        splitDirectories . dropFileName

prompt :: IO Bool
prompt = do
  putStr "\nContinue [y/N]? "
  hFlush stdout
  flip elem ["Y", "y"] <$> getLine
