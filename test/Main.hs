{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser.NWS qualified as NWS

import Text.Megaparsec qualified as Megaparsec

import Data.Time qualified as Time

import Test.Tasty.HUnit ((@?=))

import Test.Tasty (TestTree)
import Test.Tasty qualified
import Test.Tasty.HUnit qualified

main :: IO ()
main = do
    Test.Tasty.defaultMain testTree

testTree :: TestTree
testTree =
    Test.Tasty.testGroup
        "Parser"
        [climateSummaries]

climateSummaries :: TestTree
climateSummaries =
    Test.Tasty.testGroup
        "Report Time"
        [ Test.Tasty.HUnit.testCase "Day Report" $ do
            let str = "...THE SALT LAKE CITY UT CLIMATE SUMMARY FOR FEBRUARY 9 2025..."
            let result = Megaparsec.parseMaybe NWS.parseReportTime str
            case result of
                Just time -> time @?= Time.LocalTime (Time.fromGregorian 2025 2 9) (Time.TimeOfDay 23 59 59)
                Nothing -> Test.Tasty.HUnit.assertFailure $ "Parse failed: "
        , Test.Tasty.HUnit.testCase "AM Report" $ do
            let str = "...THE SALT LAKE CITY UT CLIMATE SUMMARY FOR FEBRUARY 9 2025...\nVALID TODAY AS OF 0400 AM LOCAL TIME."
            let result = Megaparsec.parseMaybe NWS.parseReportTime str
            case result of
                Just time -> time @?= Time.LocalTime (Time.fromGregorian 2025 2 9) (Time.TimeOfDay 4 0 0)
                Nothing -> Test.Tasty.HUnit.assertFailure $ "Parse failed: "
        , Test.Tasty.HUnit.testCase "PM Report" $ do
            let str = "...THE SALT LAKE CITY UT CLIMATE SUMMARY FOR FEBRUARY 9 2025...\nVALID TODAY AS OF 0400 PM LOCAL TIME."
            let result = Megaparsec.parseMaybe NWS.parseReportTime str
            case result of
                Just time -> time @?= Time.LocalTime (Time.fromGregorian 2025 2 9) (Time.TimeOfDay 16 0 0)
                Nothing -> Test.Tasty.HUnit.assertFailure $ "Parse failed: "
        ]
