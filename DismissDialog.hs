#!/usr/bin/env stack
-- stack --resolver lts-8.2 --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Turtle hiding (x)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import Data.Function
import System.Directory
import Prelude hiding (FilePath)
import System.IO hiding (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import Data.Coerce
import Data.Monoid
import Data.Char

fpToText :: FilePath -> Text
fpToText = either id id . FP.toText

detectDialog :: T.Text -> IO (Maybe (T.Text, (Int,Int)))
detectDialog winId = do
    tmpDir <- getTemporaryDirectory
    putStrLn tmpDir
    with (mktemp (fromString tmpDir) (winId <> ".png")) $ \(tmpFile, handle) -> do
        hClose handle
        (ec, _) <-
            procStrict
                "import"
                [ "-window", winId
                , fpToText tmpFile
                ]
                ""
        case ec of
            ExitFailure ec' -> do
                putStrLn $ "import: failed with " <> show ec'
                pure Nothing
            ExitSuccess -> do
                (ExitSuccess, out) <- procStrict
                    "./DetectDialog.py"
                    [ fpToText tmpFile
                    , "dialog.jpg"
                    ]
                    ""
                T.putStrLn out
                case reads (T.unpack out) :: [((Int,Int), String)] of
                    [((x,y),xs)]
                        | all isSpace xs -> pure (Just (winId, (x+458,y+123)))
                    _ -> pure Nothing

detectChrome :: Int -> IO ()
detectChrome waitTimeInSec = fix $ \self -> do
    let waitTimeInMicro = waitTimeInSec * 1000 * 1000
    (ec, resultsRaw) <-
        procStrict
            "xdotool"
            [ "search"
            , "--onlyvisible"
              -- for having a chance to match both Chrome and Chromium
            , "--classname", "Chrom"
            ] ""

    case ec of
        ExitFailure 1 -> do
            putStrLn "xdotool: no matching window detected"
            threadDelay waitTimeInMicro
            self
        ExitSuccess -> do
            -- INVARIANT: "results" is non-empty
            let results = T.lines resultsRaw
            mapM_ print results
            (coords :: [First (T.Text, (Int,Int))]) <- coerce (mapM detectDialog results)
            case getFirst (mconcat coords) of
                Nothing -> do
                    putStrLn "no dialog found."
                    threadDelay waitTimeInMicro
                    self
                Just (winId,(x,y)) -> do
                    (ExitSuccess,_) <-
                         procStrict
                             "xdotool"
                             [ "mousemove"
                             , "-window", winId
                             , fromString (show x)
                             , fromString (show y)
                             ]
                             ""
                    threadDelay (floor (0.2 * 1000 * 1000 :: Double))
                    (ExitSuccess,_) <-
                         procStrict
                             "xdotool"
                             [ "click"
                             , "1"
                             ]
                             ""
                    putStrLn "Done"
        ExitFailure ec' ->
            putStrLn $ "unexpected exitcode: " ++ show ec'

main :: IO ()
main = detectChrome 2
