{-# LANGUAGE OverloadedStrings #-}
module Transactions where

import           Control.Monad (mzero)
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as BS
import           Data.Char (isSpace)
import           Data.Csv as CSV
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.ISO8601
import qualified Data.Vector as V
import           System.IO
import           Text.Read

-- TODO: Use newtypes
data Transaction = Transaction
  { txId :: Int
  , txInstitution :: Text
  , txPaymentType :: Text
  , txAmount :: Double
  , txCurrency :: Text
  , txVendor :: Text
  , txTags :: [Text]
  , txDate :: UTCTime
  } deriving (Show, Eq)

instance CSV.FromRecord Transaction where
  parseRecord v
    | length v == 8 = v .! 7 >>= maybe mzero (mkTransaction v) . parseISO8601
    | otherwise = mzero

mkTransaction v date = Transaction <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4
                     <*> v .! 5
                     <*> (map (T.replace " " "") . (T.splitOn " ") <$> v .! 6)
                     <*> pure date

loadTransactionsFromFile :: FilePath -> IO (Either String (V.Vector Transaction))
loadTransactionsFromFile path = do
  handle <- openFile path ReadMode
  decode CSV.HasHeader <$> BS.hGetContents handle
