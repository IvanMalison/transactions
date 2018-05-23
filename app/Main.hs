{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.LocalTime
import qualified Data.Time.Zones.All as TZ
import qualified Data.Vector as V
import           Transactions

bars = ["RICKHOUSE", "P.C.H.", "BLOODHOUND", "IRISH BANK"]

addToTransactionMap :: TimeZone -> Transaction -> M.Map Day [Transaction] -> M.Map Day [Transaction]
addToTransactionMap tz transaction dayToTransactions =
  M.insert localTransactionDay (transaction:currentTransactions) dayToTransactions
  where localTransactionDateTime = utcToLocalTime tz (txDate transaction)
        localTransactionDay = localDay localTransactionDateTime
        currentTransactions = fromMaybe [] $ M.lookup localTransactionDay dayToTransactions

transactionsByDay :: V.Vector Transaction -> M.Map Day [Transaction]
transactionsByDay = foldr (addToTransactionMap pacificTimeZone) M.empty

isBarTransaction transaction =
  (elem (txVendor transaction) bars) && between6And12 (toPacific $ txDate transaction)

pacificTimeZone = utc

isBarTime = between6And12 . toPacific

toPacific = utcToLocalTime pacificTimeZone

sixOClock = midday
            -- TimeOfDay { todHour = 18 , todMinute = 0, todSecond = 0 }

between6And12 = (>= sixOClock) . localTimeOfDay

firstTripDay = fromGregorian 2012 1 23

tripEnd = fromGregorian 2012 1 26

isTripTransaction transaction = transactionDay >= firstTripDay && transactionDay < tripEnd
  where transactionLocalTime = utcToLocalTime utc $ txDate transaction
        transactionDay = localDay transactionLocalTime

main :: IO ()
main = void $ runExceptT $ do
  transactionsVector <- ExceptT $ loadTransactionsFromFile "csv_challenge.csv"
  lift $ do
     let vendorSet = getVendorSet transactionsVector
         currencySet = getCurrencySet transactionsVector
         personalTransactions = V.filter (hasTag ["personal", "food"]) transactionsVector
         barTransactions = V.filter isBarTransaction transactionsVector
         barTransactionsByDay = transactionsByDay barTransactions
         hasMultipleTransactions transactions = length transactions >= 2
         daysWithMultipleBars = filter (hasMultipleTransactions . snd)
                                (M.toList barTransactionsByDay)
         wellsFargoTransactions = V.filter onWellsFargoDebitCard transactionsVector
         tripTransactions = V.filter isTripTransaction transactionsVector
     print $ sumTransactions wellsFargoTransactions
     print $ S.size vendorSet
     print $ sumTransactions personalTransactions
     print $ sumTransactions tripTransactions
     print $ M.lookup (fst $ head daysWithMultipleBars) barTransactionsByDay
  return ()

onWellsFargoDebitCard Transaction { txInstitution = institution
                                  , txPaymentType = paymentType
                                  } =
  institution == "Wells Fargo" && paymentType == "Debit Card"

getSetBy :: (Foldable t, Ord b) => (a -> b) -> t a -> S.Set b
getSetBy itemSelector = foldr addToSet S.empty
  where addToSet transaction theSet = S.insert (itemSelector transaction) theSet

getVendorSet :: (Foldable t) => t Transaction -> S.Set T.Text
getVendorSet = getSetBy txVendor

getCurrencySet = getSetBy txCurrency

sumTransactions = foldr addTransaction 0
  where addTransaction tx v = txAmount tx + v

hasTag tags transaction = not $ null $ intersect tags (txTags transaction)


