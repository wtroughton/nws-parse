{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=))

import Data.Text.IO qualified as Text
import Data.Time qualified as Time
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as M

import Parser.NWS qualified as NWS

main :: IO ()
main = do
    Tasty.defaultMain testTree

testTree :: TestTree
testTree =
    Tasty.testGroup
        "Parser"
        [ climateSummaries
        , testReports
        ]

climateSummaries :: TestTree
climateSummaries =
    Tasty.testGroup
        "Report Time"
        [ HUnit.testCase "Day Report" $ do
            let str = "...THE SALT LAKE CITY UT CLIMATE SUMMARY FOR FEBRUARY 9 2025..."
            let result = M.parseMaybe NWS.parseReportTime str
            case result of
                Just time -> time @?= Time.LocalTime (Time.fromGregorian 2025 2 9) (Time.TimeOfDay 23 59 59)
                Nothing -> HUnit.assertFailure $ "Parse failed"
        , HUnit.testCase "AM Report" $ do
            let str = "...THE SALT LAKE CITY UT CLIMATE SUMMARY FOR FEBRUARY 9 2025...\nVALID TODAY AS OF 0400 AM LOCAL TIME."
            let result = M.parseMaybe NWS.parseReportTime str
            case result of
                Just time -> time @?= Time.LocalTime (Time.fromGregorian 2025 2 9) (Time.TimeOfDay 4 0 0)
                Nothing -> HUnit.assertFailure $ "Parse failed"
        , HUnit.testCase "PM Report" $ do
            let str = "...THE SALT LAKE CITY UT CLIMATE SUMMARY FOR FEBRUARY 9 2025...\nVALID TODAY AS OF 0400 PM LOCAL TIME."
            let result = M.parseMaybe NWS.parseReportTime str
            case result of
                Just time -> time @?= Time.LocalTime (Time.fromGregorian 2025 2 9) (Time.TimeOfDay 16 0 0)
                Nothing -> HUnit.assertFailure $ "Parse failed"
        ]

testReports :: TestTree
testReports =
    Tasty.testGroup
        "Full Report"
        [ HUnit.testCase "SLC AM Report" $ do
            report <- Text.readFile "test/data/SLC_v1.txt"
            let result = M.parseMaybe NWS.parseFullReport report
            case result of
                Just (creationTime, reportTime) -> creationTime @?= Time.LocalTime (Time.fromGregorian 2025 2 10) (Time.TimeOfDay 0 53 0)
                Nothing -> HUnit.assertFailure $ "Parse failed."
        , HUnit.testCase "LOT Precipation Timestamp" $ do
            report <- Text.readFile "test/data/LOT_v3.txt"
            let result = M.parseMaybe NWS.parseFullReport report
            case result of
                Just (creationTime, reportTime) -> creationTime @?= Time.LocalTime (Time.fromGregorian 2025 2 12) (Time.TimeOfDay 16 43 0)
                Nothing -> HUnit.assertFailure $ "Parse failed."
        ]
