{-# LANGUAGE RecordWildCards #-}
module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.Tuple
import qualified Data.Time as TF
import Data.Maybe

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)


padZeros :: Show a => Int -> a -> String
padZeros n x = take (n - length sx) (cycle "0") ++ sx 
  where sx = show x

instance Show Year where
  show Year{..} = padZeros 4 runYear
instance Show Month where
  show Month{..} = padZeros 2 runMonth
instance Show Day where
  show Day{..} = padZeros 2 runDay

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)

instance Show Hour where
  show Hour{..} = padZeros 2 runHour
instance Show Minute where
  show Minute{..} = padZeros 2 runMinute
instance Show Second where
  show Second{..} = padZeros 2 runSecond


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate
                         <*  symbol 'T'
                         <*> parseTime
                         <*> parseUTC

parseNatOfNLength :: Int -> Parser Char Int
parseNatOfNLength n = foldl (\a b -> a * 10 + b) 0 <$> sequence (replicate n newdigit)

parseNatOfLengthTwo :: Parser Char Int
parseNatOfLengthTwo = parseNatOfNLength 2

parseDate :: Parser Char Date
parseDate =
  Date <$> (Year <$> parseNatOfNLength 4)
    <*> (Month <$> parseNatOfLengthTwo)
    <*> (Day <$> parseNatOfLengthTwo)

parseTime :: Parser Char Time
parseTime = Time <$> (Hour <$> parseNatOfLengthTwo)
                <*> (Minute <$> parseNatOfLengthTwo)
                <*> (Second <$> parseNatOfLengthTwo)

parseUTC :: Parser Char Bool
parseUTC = option (True <$ symbol 'Z') False

-- Exercise 2
-- a lookup variant that searches according to the second tuple element 
lookup' :: Eq b => b -> [(a, b)] -> Maybe a
lookup' x ys = lookup x $ map swap ys

run :: Eq a => Parser a b -> [a] -> Maybe b
run p input = lookup' [] $ parse p input

-- Exercise 3
-- Record syntax documentation: https://devtut.github.io/haskell/record-syntax.html
printDateTime :: DateTime -> String
printDateTime (DateTime d t u) = printDate d ++ printTime t ++ (if u then "Z" else "")

instance Show Date where
  show = printDate

printDate :: Date -> String
printDate Date{..} = show year ++ show month ++ show day
printTime :: Time -> String
printTime Time{..} = show hour ++ show minute ++ show second

-- -- Exercise 4
parsePrint :: String -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- https://williamyaoh.com/posts/2019-09-16-time-cheatsheet.html
-- https://hackage.haskell.org/package/number-length
-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined

checkDate :: String -> Bool
checkDate s = isJust (TF.parseTimeM False TF.defaultTimeLocale "%0Y%0m%0d" s :: Maybe TF.Day)

checkTime :: String -> Bool
checkTime s = isJust (TF.parseTimeM False TF.defaultTimeLocale "%0H%M" s :: Maybe TF.TimeOfDay)
