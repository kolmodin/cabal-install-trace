{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T


data Row = Row {
  cat :: T.Text,
  pid :: Int,
  tid :: Int,
  ts :: Int,
  ph :: T.Text,
  name :: T.Text,
  id :: Maybe T.Text,
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
  raw <- T.getContents
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
          | otherwise = fromMaybe (error ("tried to end pkg that has not started: " ++ T.unpack (name r))) $ do
              n' <- lookup (name r) ps
              return (n', n':ns, [ (k,v) | (k,v) <- ps, k /= name r])

process :: T.Text -> File
process = pidFixup . File . mapMaybe parseRow . T.lines

parseRow :: T.Text -> Maybe Row
parseRow row = do
  [time_s, action, pkg] <- return (T.words row)
  Right (time, ":") <- return (T.decimal time_s)
  phase <- case action of
    "Configuring" -> return "B"
    "Installed" -> return "E"
    _ -> fail ""
  let pkg_name = T.dropWhileEnd (=='.') pkg
  return (Row "cabal-install" 0 0 time phase pkg_name Nothing (Args []))
  