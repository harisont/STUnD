{-|
Module      : GUI
Description : A quick-and-dirty GUI for L2-UD.
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}

module Main where

import Text.Read (readMaybe)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Maybe
import Data.List.Utils
import Text.PrettyPrint (render)
import UDStandard
import UDTrees
import UDPatterns
import UDVisualizations
import Utils.UDConcepts
import Utils.Output
import Align
import Match hiding (matchingSubtrees)
import Errors

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.String.Conversions
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Data.Either
import Web.Scotty
import qualified Web.Scotty as S
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (fileName, fileContent, defaultParseRequestBodyOptions)
import Network.HTTP.Types.Status (mkStatus)
import System.Directory
import System.IO.Temp

import Data.Aeson (ToJSON)
import GHC.Generics

import Debug.Trace

data Mode = TextMode | CoNNLUMode | TreeMode deriving (Eq, Read, Show, Enum)

htmlStart =
  unlines ["<!DOCTYPE html>",
            "<html lang=\"en\">",
            "<head>",
            "<meta charset=\"utf-8\">",
            "<title>STUnD trees</title>",
            "</head>",
            "<body>"
          ]

htmlEnd = 
  unlines [ "</body>",
            "</html>"
          ]

-- Result of the check_* API endpoints
data ParseStatus = Status {
  status :: Text, -- valid or invalid
  msg :: Text, -- usually empty when valid
  parsesOrErrors :: Maybe [String]
  } deriving (Generic, Show)

-- Result of the search_treebank endpoint
data AlignmentResult = Result {
  t1 :: [String],
  t2 :: [String],
  t1file :: Maybe String,
  t2file :: Maybe String,
  t1t2file :: Maybe String
  } deriving (Generic, Show)

-- Both can be serialized to JSON
instance ToJSON ParseStatus
instance ToJSON AlignmentResult

-- Directory for temporary files
-- OBS: Will be removed and re-created on startup
tmpPath :: String
tmpPath = "tmp"

-- Debug flag and debug method in the ActionM monad
debugOn = False
debug :: String -> String -> ActionM ()
debug msg var =
  if debugOn then
    liftIO $ putStrLn $ msg ++ "::\n" ++ var
  else
    return ()

-- Handler for the landing page
handleRoot :: ActionM ()
handleRoot =
  do
    -- Redirect to the stund web interface
    liftIO $ putStrLn "Redirecting"
    redirect "static/stund.html"

-- Check the validity of the query expression
checkQuery :: ActionM ()
checkQuery =
  do
    queryTxt <- queryParam "query"
    -- fieldVals is defined in UDConcepts
    let patterns = parseQuery fieldVals queryTxt
    if null patterns then
      json (Status "invalid" "could not parse query" Nothing)
    else
      json (Status "valid" "" (Just $ map show patterns))

-- Check the validity of the replacement expression
checkReplacement :: ActionM ()
checkReplacement =
  do
    replacementTxt <- queryParam "replacement"
    let replacement =  readMaybe replacementTxt :: Maybe UDReplacement
    if isNothing replacement then
      json (Status "invalid" "could not parse replacement" Nothing)
    else
      json (Status "valid" "" (Just [show $ fromJust replacement]))

-- Check the validity of a CONLL file
checkConll :: ActionM ()
checkConll =
  do
    results <- map (prsUDText . T.unpack . decodeUtf8 . fileContent . snd) <$> files
    if (all isLeft results) then
      json (Status "valid" "" (Just $ map show $ lefts results))
    else
      json (Status "invalid" "input is not in valid CoNLL-U format" $ Just $ concat $ rights results)

-- Parse an uploaded plain text file to CONLL using UDPipe API
parsePlaintext :: ActionM ()
parsePlaintext =
  do
    text "Not implemented yet"
    
-- Search the treebank(s) using the query and replacement parameters
searchTreebanks :: ActionM ()
searchTreebanks =
  do
    -- Get a map of all uploaded files from filename to file info
    formFiles <- M.fromList <$> files
    -- Get the file mode
    mode <- read <$> formParam "mode"
    -- By default do not highlight "diff"
    diff <- fromMaybe False <$> fmap read <$> formParamMaybe "diff"
    t1file <- maybeTmpFile <$> formParamMaybe "t1file"
    liftIO $ putStrLn $ show t1file
    t2file <- maybeTmpFile <$> formParamMaybe "t2file"
    t1t2file <- maybeTmpFile <$> formParamMaybe "t1t2file"
    -- Get text for both files
    let t1Text = decodeUtf8 $ fileContent $ formFiles M.! "treebank1"
    let t2Text = decodeUtf8 $ fileContent $ formFiles M.! "treebank2"
    -- Get pattern and replacement
    queryTxt <- formParam "query"
    replacementTxt <- formParam "replacement"
    let patterns = if null queryTxt
                   then [(DEPREL_ "root",DEPREL_ "root")]
                   else parseQuery fieldVals queryTxt
    let mreplacement = if null replacementTxt
                       then Just $ CHANGES []
                       else readMaybe replacementTxt
    -- Convert to sentences
    let t1Sents = prsUDText $ T.unpack t1Text
    -- If the treebank 2 is empty, fill with dummy sentences
    -- (better than using the treebank 1 again, because alignment complexity
    -- will be negligible if the trees are empty) 
    let t2Sents = if (not . null . T.unpack) t2Text 
                    then prsUDText $ T.unpack t2Text 
                    else Left $ repeat (tree2sentence dummyUDTree)
    -- Align sentences
    let treebank = (fromLeft [] t1Sents) `zip` (fromLeft [] t2Sents)
    let alignments = map align treebank
    -- true bilingual matches
    let bimatches = treebank `zip` map (match patterns) alignments
    -- all matches (add treebank 1-only with dummy alignments)
    let matches = map
          (\bms@((s1,s2),ms) ->
             let pattern = patterns !! 0
             in if isMonolingual pattern
                  then ((s1,s2), ms ++ zip (filter 
                    (\t -> not $ t `elem` (map fst ms))
                    (matchingSubtrees (fst $ (pattern)) (sentence2tree s1)))
                      (repeat $ dummyUDTree))
                else bms)
          bimatches
    let matches' =
          map
            (\(s,es) ->
               (s,map (applyReplacement (fromJust $ mreplacement)) es))
            (filter (\(_,ms) -> not $ null ms) matches)
    let (t1Col,t2Col) =
          unzip $ concatMap
            (\((s1,s2),ms) ->
                map
                  (\(m1,m2) ->
                      let m1' =
                            if m1 == dummyUDTree
                            then tree2sentence m1
                            else tree2sentence (subtree2tree m1)
                          m2' = tree2sentence (subtree2tree m2)
                          mark content = 
                            if diff && m1 /= m2 && isJust t2file
                              then "<mark>" ++ content ++ "</mark>"
                              else content
                      in ((mark (case mode of
                              TextMode -> highlin s1 (tree2sentence m1) HTML
                              CoNNLUMode -> (prt m1') ++ "\n"
                              TreeMode -> sentence2svgFragment $ m1'),
                          mark (case mode of
                             TextMode -> highlin s2 (tree2sentence m2) HTML
                             CoNNLUMode -> (prt m2') ++ "\n"
                             TreeMode -> sentence2svgFragment $ m2')))) 
                  ms)
            matches'
    t1t2Tmpfile <- liftIO $ writeMaybeTempFile t1t2file "t1-t2-.tsv" $ unlines $ map
        (\(t1,t2) -> t1 ++ "\t" ++ t2)
        ((map (rmMarkup . mkUpper) t1Col) `zip` (map (rmMarkup . mkUpper) t2Col))
    t1Tmpfile <- liftIO $ (\(fname, fdata) -> writeMaybeTempFile t1file fname fdata) $ case mode of { TextMode -> ("t1-.txt", rmMarkup $ mkUpper $ unlines t1Col) ; CoNNLUMode -> ("t1-.conllu", rmMarkup $ unlines t1Col) ; TreeMode -> ("t1-.html", htmlStart ++ (rmMarkup $ unlines $ L.intersperse "\n<br>\n" t1Col) ++ htmlEnd) }
    t2Tmpfile <- liftIO $ (\(fname, fdata) -> writeMaybeTempFile t1file fname fdata) $ case mode of { TextMode -> ("t2-.txt", rmMarkup $ mkUpper $ unlines t2Col) ; CoNNLUMode -> ("t2-.conllu", rmMarkup $ unlines t2Col) ; TreeMode -> ("t2-.html", htmlStart ++ (rmMarkup $ unlines $ L.intersperse "\n<br>\n" t2Col) ++ htmlEnd) }
    json $ if (not . null . T.unpack) t2Text  
      then Result { -- parallel treebank
        t1 = t1Col, 
        t2 = t2Col, 
        t1file = Just t1Tmpfile, 
        t2file = Just t2Tmpfile, 
        t1t2file = Just t1t2Tmpfile }
      else Result { -- single treebank
        t1 = t1Col,
        t2 = [],
        t1file = Just t1Tmpfile,
        t2file = Nothing,
        t1t2file = Nothing }
      where
        -- remove markup tags <b> and <mark>
        rmMarkup s = replace "</mark>" "" $ replace "<mark>" "" $ replace "</b>" "" $ replace "<b>" "" s
        -- replace <b>text</b> markup by upppercase TEXT
        mkUpper s
          | L.isPrefixOf "<b>" s = mkUpper' $ drop 3 s
          | null s = ""
          | otherwise = head s:mkUpper (tail s)
          where
            mkUpper' s
              | L.isPrefixOf "</b>" s = mkUpper $ drop 4 s
              | otherwise = toUpper (head s):mkUpper' (tail s)
        -- Writes the content either to a given file if it exists or to a new temporary file otherwise
        writeMaybeTempFile :: Maybe FilePath -> String -> String -> IO FilePath
        writeMaybeTempFile maybeFile tmpFilePattern content =
          if isJust maybeFile then
              do
                let fn = fromJust maybeFile
                writeFile fn content
                return fn
          else
            writeTempFile tmpPath tmpFilePattern content
        -- Checks the content of a Maybe String and make it Nothing if the String is empty or invalid
        maybeTmpFile :: Maybe String -> Maybe FilePath
        maybeTmpFile Nothing = Nothing
        maybeTmpFile (Just []) = Nothing
        -- Check if it is a file within the tmpPath
        maybeTmpFile (Just s)
          | L.isPrefixOf tmpPath s = Just s
          | otherwise = Nothing
        applyReplacement r (e1,e2) =
          (fst $ replacementsWithUDPattern r e1,
           fst $ replacementsWithUDPattern r e2)

-- Downloads a temp file
downloadTmpFile :: ActionM ()
downloadTmpFile =
  do
    fileName <- queryParam "filename"
    if L.isPrefixOf tmpPath fileName then
      do
        if L.isSuffixOf "html" fileName then
          setHeader "Content-Type" "text/html; charset=utf-8"
        else
          setHeader "Content-Type" "text/plain; charset=utf-8"
        setHeader "Content-Disposition:" $ convertString $ "attachment; filename=\"" ++ fileName ++ "\""
        file fileName 
    else
      S.status $ mkStatus 403 "Access denied"

-- Main method for the server
main :: IO ()
main =
  do
    -- Cleanup old temporary directory if it exists
    tmpExists <- doesDirectoryExist tmpPath
    if tmpExists then
      removeDirectoryRecursive tmpPath
    else
      return ()
    -- Create directory for temporary files
    createDirectory tmpPath
    -- Start the web server
    scotty 3000 $
      do
        -- Logs requests
        middleware logStdoutDev
        -- Handles static files
        middleware static
        get "/" $ handleRoot
        get "/index.html" $ handleRoot
        get "/check_query" $ checkQuery
        get "/check_replacement" $ checkReplacement
        post "/check_conll" $ checkConll
        post "/parse_plaintext" $ parsePlaintext
        post "/search_treebanks" $ searchTreebanks
        get "/tmp_file/" $ downloadTmpFile
