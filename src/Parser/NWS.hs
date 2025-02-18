{-# LANGUAGE OverloadedStrings #-}

module Parser.NWS
    ( parseCreationTime
    , parseFullReport
    , parseReportHeader
    , parseReportTime
    , parseMonth
    , skipSepLine
    , ReportTime
    , CreationTime
    ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)

import Data.Text.IO qualified as Text

import Control.Monad.Combinators qualified as M
import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Time qualified as Time
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = M.Parsec Void Text

type ReportTime = Time.LocalTime

type CreationTime = Time.LocalTime

parseFullReport :: Parser (CreationTime, ReportTime)
parseFullReport = do
    creationTime <- parseReportHeader
    skipSepLine
    M.space1
    reportTime <- parseReportTime
    return (creationTime, reportTime)

parseCreationTime :: Parser CreationTime
parseCreationTime = do
    -- Parse the next 7 space-separated values
    values <- M.count 7 $ M.takeWhile1P Nothing (not . Char.isSpace) <* M.space

    -- Combine into single string
    let timeStr = Text.unpack $ Text.unwords values

    -- Parse the combined string into LocalTime
    let format = "%-I%M %p %Z %a %b %d %Y"
    case Time.parseTimeM True Time.defaultTimeLocale format timeStr :: Maybe Time.LocalTime of
        Just localTime -> return localTime
        Nothing -> fail $ "Failed to parse time: " ++ timeStr

parseReportTime :: Parser ReportTime
parseReportTime = do
    -- Skip prefix (e.g., "...THE SALT LAKE CITY UT ClIMATE SUMMARY FOR")
    skipUntilClimateSummary
    void $ M.string "CLIMATE SUMMARY FOR"
    void M.space

    -- Parse the date (e.g., "JANUARY 30 2025")
    month <- parseMonth
    void M.space
    day <- L.decimal
    void M.space
    year <- L.decimal
    void $ M.string "..."

    -- Parse the optional time (e.g. VALID TODAY AS OF 0400 PM LOCAL TIME)
    timeOfDay <- M.optional . M.try $ do
        void M.space
        void $ M.string "VALID TODAY AS OF"
        void M.space
        hourMinute <- M.count 4 M.digitChar
        let hour = read [hourMinute !! 0, hourMinute !! 1] :: Int
        let minute = read [hourMinute !! 2, hourMinute !! 3] :: Int
        void M.space
        amPm <- Text.toUpper <$> M.takeWhile1P Nothing (/= ' ')
        void M.space
        void $ M.string "LOCAL TIME."
        return $ Time.TimeOfDay (if amPm == "PM" && hour < 12 then hour + 12 else hour) minute 0

    -- Default to 23:59:59 if time is missing
    let defaultTime = Time.TimeOfDay 23 59 59
    let time = Maybe.fromMaybe defaultTime timeOfDay

    let dayOfMonth = Time.fromGregorian year month day
    return $ Time.LocalTime dayOfMonth time

skipUntilClimateSummary :: Parser ()
skipUntilClimateSummary = do
    -- Look ahead for CLIMATE SUMMARY
    let str = "CLIMATE SUMMARY"
    input <- M.getInput
    case Text.breakOn str input of
        (_, "") -> fail $ "Could not find " ++ (Text.unpack str)
        -- Skip the part before CLIMATE SUMMARY
        (_, rest) -> do
            void $ M.takeP Nothing (Text.length input - Text.length rest)

skipSepLine :: Parser ()
skipSepLine = do
    M.skipSome (M.char '.')
    void M.eol
    pure ()

parseReportHeader :: Parser CreationTime
parseReportHeader = do
    M.space1
    _ <- M.count 3 $ M.skipManyTill M.anySingle M.eol
    M.space1
    _ <- M.string "CLIMATE REPORT"
    void M.eol
    _ <- M.string "NATIONAL WEATHER SERVICE"
    _ <- M.skipManyTill M.anySingle M.eol
    parseCreationTime

parseMonth :: Parser Int
parseMonth = do
    monthStr <- Text.toUpper <$> M.takeWhile1P Nothing (/= ' ')
    case monthStr of
        "JANUARY" -> return 1
        "FEBRUARY" -> return 2
        "MARCH" -> return 3
        "APRIL" -> return 4
        "MAY" -> return 5
        "JUNE" -> return 6
        "JULY" -> return 7
        "AUGUST" -> return 8
        "SEPTEMBER" -> return 9
        "OCTOBER" -> return 10
        "NOVEMBER" -> return 11
        "DECEMBER" -> return 12
        _ -> fail $ "Invalid month: " ++ Text.unpack monthStr
