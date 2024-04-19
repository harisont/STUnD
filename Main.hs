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

import UDConcepts
import UDPatterns
import VisualizeUD
import Utils.UDConcepts
import Utils.Output
import Align
import Match hiding (matchesUDPattern)
import Errors

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import qualified Data.List as L
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

data ParseStatus = Status {
  status :: Text,
  msg :: Text,
  patterns :: Maybe [String]
  } deriving (Generic, Show)

data AlignmentResult = Result {
  l1 :: [String],
  l2 :: [String],
  l1file :: String,
  l2file :: String,
  l1l2file :: String
  } deriving (Generic, Show)

instance ToJSON ParseStatus
instance ToJSON AlignmentResult

-- Directory for temporary files
-- OBS: Will be removed and re-created on startup
tmpPath :: String
tmpPath = "tmp"

debugOn = False
debug :: String -> String -> ActionM ()
debug msg var =
  if debugOn then
    liftIO $ putStrLn $ msg ++ "::\n" ++ var
  else
    return ()
    
handleRoot :: ActionM ()
handleRoot =
  do
    liftIO $ putStrLn "Redirecting"
    redirect "/static/stund.html"

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

checkReplacement :: ActionM ()
checkReplacement =
  do
    replacementTxt <- queryParam "replacement"
    -- fieldVals is defined in UDConcepts
    let replacement =  readMaybe replacementTxt
    if isNothing replacement then
      json (Status "invalid" "could not parse replacement" Nothing)
    else
      json (Status "valid" "" replacement)

searchTreebanks :: ActionM ()
searchTreebanks =
  do
    -- Get a map of all uploaded files from filename to file info
    formFiles <- M.fromList <$> files
    -- Get tehe file mode
    mode <- read <$> formParam "mode"
    -- Get text for both files
    let l1Text = decodeUtf8 $ fileContent $ formFiles M.! "l1treebank"
    let l2Text = decodeUtf8 $ fileContent $ formFiles M.! "l2treebank"
    liftIO $ putStrLn $ T.unpack l2Text
    -- Get pattern and replacement
    queryTxt <- formParam "query"
    replacementTxt <- formParam "replacement"
    let patterns = if null queryTxt
                   then [(DEPREL_ "root",DEPREL_ "root")]
                   else parseQuery fieldVals queryTxt
    let mreplacement = if null replacementTxt
                       then Just $ CHANGES []
                       else readMaybe replacementTxt
    liftIO $ putStrLn $ show patterns
    -- Convert to sentences. If the L1 treebank is empty use a copy of the L2 treebank
    let l1Sents = if (not . null . T.unpack) l1Text then parseUDText $ T.unpack l1Text else parseUDText $ T.unpack l2Text
    let l2Sents = parseUDText $ T.unpack l2Text
    -- Align sentences
    let treebank = l1Sents `zip` l2Sents
    let alignments = map align treebank
    -- true bilingual matches
    let bimatches = treebank `zip` map (match patterns) alignments
    -- all matches (add L2-only with dummy alignments)
    let matches =
          map
            (\bms@((s1,s2),ms) ->
               let pattern = patterns !! 0
               in
                 if isL2only $ pattern
                 then ((s1,s2), ms ++ ((repeat $ dummyUDTree) `zip` filter
                                       (\t -> not $ t `elem` (map snd ms))
                                        (matchesUDPattern (snd $ (pattern)) (udSentence2tree s2))))
                 else bms)
            bimatches
    let matches' =
          map
            (\(s,es) ->
               (s,map (applyReplacement (fromJust $ mreplacement)) es))
            (filter (\(_,ms) -> not $ null ms) matches)
    let (l1Col,l2Col) =
          unzip $ concatMap
            (\((s1,s2),ms) ->
                map
                  (\(m1,m2) ->
                      let m1' =
                            if m1 == dummyUDTree
                            then udTree2sentence m1
                            else udTree2sentence (adjustRootAndPositions m1)
                          m2' = udTree2sentence (adjustRootAndPositions m2)
                      in ((case mode of
                              TextMode -> highlin s1 (udTree2sentence m1) HTML
                              CoNNLUMode -> (prt m1') ++ "\n"
                              TreeMode -> render $ conll2svg $ prt m1',
                           case mode of
                             TextMode -> highlin s2 (udTree2sentence m2) HTML
                             CoNNLUMode -> (prt m2') ++ "\n"
                             TreeMode -> render $ conll2svg $ prt m2')
                         ))
                  ms)
            matches'
    l1l2Tmpfile <- liftIO $ writeTempFile tmpPath "l1-l2-.tsv" $
      unlines $ map
        (\(l1,l2) -> l1 ++ "\t" ++ l2)
        ((map rmBold l1Col) `zip` (map rmBold l2Col))
    l1Tmpfile <- liftIO $ writeTempFile tmpPath "l1-.htm" $ unlines l1Col
    l2Tmpfile <- liftIO $ writeTempFile tmpPath "l2-.htm" $ unlines l2Col
--           -- destroyTables window
--   --   table <- buildTable window l1Col l2Col mode
--   --     -- tree mode (currently) behaves as CoNNL-U mode in terms of export
--   --   if mode == TextMode then unhide tsvDlSpan else unhide treebankDlSpan
--   --   element nHitsSpan # set text ((show $ length l1Col) ++ " hits")
--   --   unhide nHitsSpan
--   --   getBody window #+ [element table, element nHitsSpan]
    json $ Result { l1 = l1Col, l2 = l2Col, l1file = l1Tmpfile, l2file = l2Tmpfile, l1l2file = l1l2Tmpfile }
      where
        rmBold s = replace "</b>" "" (replace "<b>" "" s)
        applyReplacement r (e1,e2) =
          (fst $ replacementsWithUDPattern r e1,
           fst $ replacementsWithUDPattern r e2)

downloadTmpFile :: ActionM ()
downloadTmpFile =
  do
    fileName <- queryParam "filename"
    if L.isPrefixOf tmpPath fileName then
      do
        setHeader "Content-Type" "text/plain; charset=utf-8"
        file fileName 
    else
      S.status $ mkStatus 403 "Access denied"
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
        get "/check_query" $ checkQuery
        get "/check_replacement" $ checkReplacement
        post "/search_treebanks" $ searchTreebanks
        get "/tmp_file/" $ downloadTmpFile

      
-- buildTable :: Window -> [String] -> [String] -> Mode -> UI Element
-- buildTable window l1Data l2Data mode = do 
--   cells <- mapM (mapM 
--                   (return . (\htmlText -> do
--                     div <- UI.div
--                     element div # set html htmlText
--                     element div # set UI.style [
--                         ("text-align", "left")
--                       , ("padding", "8px")
--                       , ("white-space", "pre-wrap")
--                       , ("overflow", "auto")
--                       , ("font-family", if mode == CoNNLUMode 
--                                           then "monospace, monospace"
--                                           else "inherit")
--                       ]
--                     return div)))
--             (zipWith (\s1 s2 -> [s1,s2]) l1Data l2Data)
--   table <- UI.grid cells
--   element table # set UI.style [("width","100%"), ("table-layout","fixed")]
--   return table
