{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics
import Control.Monad

import Data.Aeson
import qualified Data.Text as T

data Row = Row {
  cat :: String,
  pid :: Int,
  tid :: Int,
  ts :: Int,
  ph :: String,
  name :: String,
  args :: Args
} deriving Generic

data File = File [Row] deriving Generic

data Args = Args [(T.Text, Value)]

instance ToJSON Row
instance ToJSON File

instance ToJSON Args where
  toJSON (Args pairs) = object [ k .= v | (k,v) <- pairs]

main :: IO ()
main = do
  raw <- getContents
  B.putStrLn (encode (process raw))

pidFixup :: File -> File
pidFixup (File rows) = File (go ([0..], []) rows)
  where
    go _ [] = []
    go (ns, ps) (r:rs) = r { tid = n } : r' : go (ns', ps') rs
      where
        r' = r { name = "parallel", pid = 1, ph = "C", args = Args [("", toJSON (length ps'))]}
        (n,ns',ps')
          | ph r == "B" = (head ns, tail ns, (name r, head ns):ps)
          | otherwise = fromMaybe (error ("tried to end pkg that has not started: " ++ name r)) $ do
              n' <- lookup (name r) ps
              return (n', n':ns, [ (k,v) | (k,v) <- ps, k /= name r])

process :: String -> File
process = pidFixup . File . mapMaybe processRow . lines

processRow :: String -> Maybe Row
processRow row = do
  [time_s, action, pkg] <- return (words row)
  guard ((not . null) time_s && last time_s == ':')
  let time_as_string = init time_s
  guard (all isNumber time_as_string)
  let t = read time_as_string
  phase <- case action of
    "Configuring" -> return "B"
    "Installed" -> return "E"
    _ -> fail ""
  let pkg_name = reverse . dropWhile (=='.') . reverse $ pkg
  return (Row "cabal-install" 0 0 t phase pkg_name (Args []))
