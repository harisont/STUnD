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
import Data.List
import Data.Bifunctor
import Text.PrettyPrint (render)
import RTree
import UDStandard
import UDTrees
import UDPatterns
import UDVisualizations
import Utils.UDConcepts
import Utils.Output
import Utils.Misc
import Align
import Match hiding (matchingSubtrees)
import Errors
import Extract

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Data.Either
import Web.Scotty
import qualified Web.Scotty as S
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (fileName,fileContent,defaultParseRequestBodyOptions)
import Network.HTTP.Types.Status (mkStatus)
import System.Directory
import System.IO.Temp

import Data.Aeson (ToJSON)
import GHC.Generics

import Debug.Trace

data Mode = TextMode | CoNNLUMode | TreeMode deriving (Eq, Read, Show, Enum)

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
  h1 :: [[Int]], -- highlights t1
  h2 :: [[Int]], -- highlights t2
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
    replTxt <- queryParam "replacement"
    let repl =  readMaybe replTxt :: Maybe UDReplacement
    if isNothing repl then
      json (Status "invalid" "could not parse replacement" Nothing)
    else
      json (Status "valid" "" (Just [show $ fromJust repl]))

-- Check the validity of a CONLL file
checkConll :: ActionM ()
checkConll =
  do
    results <- map (prsUDText . T.unpack . decodeUtf8 . fileContent . snd) <$> files
    if all isLeft results then
      json (Status "valid" "" (Just $ map show $ lefts results))
    else
      json (Status "invalid" "input is not in valid CoNLL-U format" $ Just $ concat $ rights results)

-- | A match is a pair that associates a sentence pair with a list of 
-- alignments matching the query (could/should be moved to L2-UD; the match
-- function should return this directly)
type Match = ((UDTree,UDTree), [Alignment])

-- | STUnD's rather permissive matching function, allowing both bilingual and
-- T1-only matches for monolingual patterns 
stundMatch :: [(UDTree,UDTree)] -> [(UDPattern,UDPattern)] -> [Match]
stundMatch ts ps = filter (\(_,ms) -> not $ null ms) $
  if isMonolingual p then map addDummies bimatches else bimatches
  where 
    as = map align ts :: [[(UDTree,UDTree)]]
    -- proper "bilingual" matches (the pattern is matched both on T1 and T2)
    bimatches = ts `zip` map (match ps) as
    p = head ps
    -- go through proper matches and add (T1-match,dummy) alignments when a
    -- the query is monolingual and matches T1 without a clear correspondence
    -- in T2
    addDummies :: Match -> Match
    addDummies bm@((t1,t2),ms) = ((t1,t2), ms ++ (monomatches `zip` dummies))
      where 
        monomatches = filter 
          (\t -> t `notElem` map fst ms) 
          (matchingSubtrees (fst p) t1)
        dummies = repeat dummyUDTree

stundReplace :: [Match] -> UDReplacement -> [Match]
stundReplace matches r = map (second (map (applyReplacement r))) matches 
  where applyReplacement r (e1,e2) =
          (fst $ replacementsWithUDPattern r e1,
           fst $ replacementsWithUDPattern r e2)

-- Search the treebank(s) using the query and replacement parameters
searchTreebanks :: ActionM ()
searchTreebanks = do
  formFiles <- M.fromList <$> files
  let t1Text = decodeUtf8 $ fileContent $ formFiles M.! "treebank1"
  let t2Text = decodeUtf8 $ fileContent $ formFiles M.! "treebank2"
  let t1Sents = prsUDText $ T.unpack t1Text
  -- if T2 is empty, fill with dummy sentences
  let t2Sents = if (not . null . T.unpack) t2Text 
                  then prsUDText $ T.unpack t2Text 
                  else Left $ repeat (tree2sentence dummyUDTree)
  let treebank = map   
                  (bimap sentence2tree sentence2tree) 
                  (fromLeft [] t1Sents `zip` fromLeft [] t2Sents)

  mode <- read <$> formParam "mode"
  diff <- maybe False read <$> formParamMaybe "diff"

  t1File <- maybeTmpFile <$> formParamMaybe "t1file"
  t2file <- maybeTmpFile <$> formParamMaybe "t2file"
  t1t2file <- maybeTmpFile <$> formParamMaybe "t1t2file"

  queryTxt <- formParam "query"
  let patterns = if null queryTxt
                 then [(DEPREL_ "root",DEPREL_ "root")]
                 else parseQuery fieldVals queryTxt
                
  replTxt <- formParam "replacement"
  let repl = fromJust $ if null replTxt
                     then Just $ CHANGES []
                     else readMaybe replTxt
  
  let matches = stundReplace (stundMatch treebank patterns) repl
  
  let divergences = if diff -- skip if not in diff mode to save time
        then concatMap (map 
          (((minimal . extractDivergences) -- 4. extract "minimal" divergences
            . align) -- 3. re-align the adjusted matching subtrees
            . bimap subtree2tree subtree2tree) -- 2. adjust indices
            . snd) -- 1. take matching subtrees
          matches 
        else []
  let (t1Col,t2Col) =
        unzip $ concatMap
          (\((t1,t2),ms) ->
              map
                (\(m1,m2) ->
                    let m1' =
                          if m1 == dummyUDTree
                          then tree2sentence m1
                          else tree2sentence $ subtree2tree m1
                        m2' = tree2sentence $ subtree2tree m2
                    in ((case mode of
                            TextMode -> highlin 
                                          (tree2sentence t1) 
                                          (tree2sentence m1) HTML
                            CoNNLUMode -> prt m1' ++ "\n"
                            TreeMode -> sentence2svgFragment m1',
                        case mode of
                           TextMode -> highlin 
                                        (tree2sentence t2) 
                                        (tree2sentence m2) HTML
                           CoNNLUMode -> prt m2' ++ "\n"
                           TreeMode -> sentence2svgFragment m2'))) 
                ms)
          matches
  t1t2Tmpfile <- liftIO $ 
                  writeMaybeTempFile t1t2file "t1-t2-.tsv" $ unlines $ map
      (\(t1,t2) -> t1 ++ "\t" ++ t2)
      ((map rmMarkup t1Col) `zip` (map rmMarkup t2Col))
  t1Tmpfile <- liftIO $ writeMaybeTempFile t1File "t1-.htm" $ 
                case mode of { 
                  TextMode -> rmMarkup $ mkUpper $ unlines t1Col ; 
                  _ -> rmMarkup $ unlines t1Col }
  t2Tmpfile <- liftIO $ writeMaybeTempFile t2file "t2-.htm" $ 
                case mode of { 
                  TextMode -> rmMarkup $ mkUpper $ unlines t2Col ; 
                  _ -> rmMarkup $ unlines t2Col }

  -- transform divergences into indices needed to highlight words in diff
  -- mode in the frontend
  let divIndices = unzip $ map 
        ((bimap words2ints words2ints . unzip) -- 2. UDWords -> udIDs -> Ints
        . map (bimap allNodes allNodes)) -- 1. aligned UDTree -> [UDWords]
        divergences                  
  json $ if (not . null . T.unpack) t2Text  
    then Result { -- parallel treebank
      t1 = t1Col, 
      t2 = t2Col, 
      h1 = fst divIndices,
      h2 = snd divIndices,
      t1file = Just t1Tmpfile, 
      t2file = Just t2Tmpfile, 
      t1t2file = Just t1t2Tmpfile }
    else Result { -- single treebank
      t1 = t1Col,
      t2 = [],
      h1 = [],
      h2 = [],
      t1file = Just t1Tmpfile,
      t2file = Nothing,
      t1t2file = Nothing }
    where
      words2ints = map (id2int . udID) . rmDuplicates . concat
      -- undo HTML injection into SVG, replace with proper attribute
      rmMarkup s = replace "</b>" "" $ replace "<b>" "" s
      -- replace <b>text</b> markup by uppercase TEXT
      mkUpper s
        | L.isPrefixOf "<b>" s = mkUpper' $ drop 3 s
        | null s = ""
        | otherwise = head s:mkUpper (tail s)
        where
          mkUpper' s
            | L.isPrefixOf "</b>" s = mkUpper $ drop 4 s
            | otherwise = toUpper (head s):mkUpper' (tail s)
      -- Writes the content either to a given file if it exists or to a new 
      -- temporary file otherwise
      writeMaybeTempFile :: Maybe FilePath -> String -> String -> IO FilePath
      writeMaybeTempFile maybeFile tmpFilePattern content =
        if isJust maybeFile then
            do
              let fn = fromJust maybeFile
              writeFile fn content
              return fn
        else
          writeTempFile tmpPath tmpFilePattern content
      -- Checks the content of a Maybe String and make it Nothing if the 
      -- String is empty or invalid
      maybeTmpFile :: Maybe String -> Maybe FilePath
      maybeTmpFile Nothing = Nothing
      maybeTmpFile (Just []) = Nothing
      -- Check if it is a file within the tmpPath
      maybeTmpFile (Just s)
        | L.isPrefixOf tmpPath s = Just s
        | otherwise = Nothing


-- Downloads a temp file
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
        get "/check_repls" $ checkReplacement
        post "/check_conll" $ checkConll
        post "/search_treebanks" $ searchTreebanks
        get "/tmp_file/" $ downloadTmpFile
