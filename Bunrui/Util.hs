module Bunrui.Util where

import Prelude hiding (any, foldr)
import Control.Applicative ((<$>))
import Data.Foldable  (Foldable, any, foldr)
import Data.List      (inits)
import Control.Monad  (MonadPlus, mplus, mzero, unless)
import System.Exit    (ExitCode(..))
import System.FilePath (dropFileName, joinPath, splitDirectories)
import System.Process (readProcessWithExitCode)
import System.IO      (hFlush, stdout)


(<.>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f <.> g) x y = f (g x y)

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

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond th = cond >>= flip unless th

elemBy :: Foldable f => (a -> b -> Bool) -> a -> f b -> Bool
elemBy = (any .)

nubBy :: (MonadPlus m, Foldable m) => (a -> a -> Bool) -> m a -> m a
nubBy eq = foldr f mzero
    where f x a = (if elemBy eq x a then mzero else return x) `mplus` a

leadingPathComponents :: FilePath -> [FilePath]
leadingPathComponents = drop 1 . map joinPath . inits .
                        splitDirectories . dropFileName

prompt :: IO Bool
prompt = do
  putStr "\nContinue [y/N]? "
  hFlush stdout
  flip elem ["Y", "y"] <$> getLine
