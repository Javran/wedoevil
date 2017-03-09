#!/usr/bin/env stack
-- stack --resolver lts-8.2 --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Turtle hiding (x, f, some, fp)
import qualified Data.Text as T
import Control.Concurrent
import Data.Function
import System.Directory
import Prelude hiding (FilePath)
import System.IO hiding (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import Data.Char
import Data.Maybe
import Control.Applicative
import qualified Text.ParserCombinators.ReadP as P
import Control.Monad
import qualified Control.Foldl as Fold

type WindowId = T.Text

fpToText :: FilePath -> Text
fpToText = either id id . FP.toText

firstSuccessfulM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstSuccessfulM [] = pure Nothing
firstSuccessfulM (m:ms) = m >>= f
  where
    f (Just v) = pure (Just v)
    f Nothing = firstSuccessfulM ms

detectDialog :: T.Text -> T.Text -> IO (Maybe (Int,Int))
detectDialog patFile winId = do
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
                    , patFile
                    ]
                    ""
                case reads (T.unpack out) :: [((Int,Int), String)] of
                    [(loc,xs)]
                        | all isSpace xs -> pure (Just loc)
                    _ -> pure Nothing

detectChrome :: Double -> (WindowId -> IO (Maybe ())) -> IO ()
detectChrome waitTimeInSec action = fix $ \self -> do
    let waitAndRetry = threadDelay waitTimeInMicro >> self
          where
            waitTimeInMicro = floor (waitTimeInSec * 1000 * 1000)
    (ec, resultsRaw) <-
        procStrict
            "xdotool"
            [ "search"
            , "--onlyvisible"
              -- for having a chance to match both Chrome and Chromium
            , "--classname", "Chrom"
            ] ""

    case ec of
        ExitFailure 1 ->
            putStrLn "xdotool: no matching window detected" >> waitAndRetry
        ExitSuccess -> do
            -- INVARIANT: "results" is non-empty
            let results = T.lines resultsRaw
            mcoord <- firstSuccessfulM (map action results)
            case mcoord of
                Nothing ->
                    putStrLn "no dialog found." >> waitAndRetry
                Just () ->
                    putStrLn "Done"
        ExitFailure ec' ->
            putStrLn $ "unexpected exitcode: " ++ show ec'

detectAndDismissDialog :: [(T.Text,(Int,Int))] -> WindowId -> IO (Maybe ())
detectAndDismissDialog pats winId = do
    let detect' (patFile, (cancelDX,cancelDY)) =
            (fmap. fmap)
                (\(x,y) -> (x+cancelDX,y+cancelDY))
                (detectDialog patFile winId)
    result <- firstSuccessfulM (map detect' pats)
    case result of
        Just (x,y) -> do
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
            pure (Just ())
        Nothing -> pure Nothing

getCancelLoc :: String -> Maybe (Int, Int)
getCancelLoc xs = case P.readP_to_S pCancelLoc xs of
    [(loc,"")] -> Just loc
    _ -> Nothing
  where
    pCancelLoc :: P.ReadP (Int,Int)
    pCancelLoc = do
        _ <- some (P.satisfy (const True))
        [d1,d2] <- replicateM 2 (P.char '_' >> P.munch1 isDigit)
        P.eof
        pure (read d1, read d2)

main :: IO ()
main = do
    let toPatternInfo fp = do
            ext <- extension fp
            guard (T.toLower ext `elem` ["png","jpg","bmp"])
            loc <- getCancelLoc (FP.encodeString $ basename fp)
            pure (fpToText fp, loc)
    xs <- fold (ls "sample") Fold.list
    let ys = mapMaybe toPatternInfo xs
    case ys of
        [] -> error "no sample file detected."
        _ -> do
            putStrLn "Sample files:"
            mapM_ print ys
            detectChrome 0.2 (detectAndDismissDialog ys)
