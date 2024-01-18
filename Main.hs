{-|
Module      : GUI
Description : A quick-and-dirty GUI for L2-UD.
Stability   : experimental
-}

module Main where 

import Prelude hiding (readFile, writeFile)
import Data.ByteString (readFile, writeFile)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Read (readMaybe)
import Data.Maybe
import Data.List.Utils
import System.Directory
import Text.PrettyPrint (render)
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import UDConcepts
import UDPatterns
import VisualizeUD
import Utils.UDConcepts
import Utils.Output
import Align
import Match hiding (matchesUDPattern)
import Errors

data Mode = TextMode | CoNNLUMode | TreeMode deriving (Eq, Show)

main :: IO ()
main = do
  startGUI defaultConfig
    { jsPort = Just 8023
    , jsStatic = Just "static"
    } setup

setup :: Window -> UI ()
setup window = do
  let path = "tmp"
  let path1 = "tmp1"
  let path2 = "tmp2"
  uri <- loadFile "text/plain" path
  uri1 <- loadFile "text/plain" path1
  uri2 <- loadFile "text/plain" path2

  return window # set UI.title "STUnD"
  
  globalStyle <- mkElement "style"
  element globalStyle # 
    set text (unlines [
                ".table-row:nth-child(even) {background-color: #dddddd}"
              , "body {font-family: arial, sans-serif}"
              , ".unselectable {"
              , "  -webkit-touch-callout: none;"
              , "  -webkit-user-select: none;"
              , "  -khtml-user-select: none;"
              , "  -moz-user-select: none;"
              , "  -ms-user-select: none;"
              , "  user-select: interspersenone;}"])

  l1Input <- buildTextInput "path to L1 treebank" "path"
  element l1Input # set UI.style [("width","49.4%")]
  l2Input <- buildTextInput "path to L2 treebank" "path"
  element l2Input # set UI.style [("width","49.4%")]

  queryInput <- buildTextInput
                  "L1-L2 or single-language (matched on L2) query"
                  "query"
  element queryInput # set UI.style [("width","99.2%")]

  searchButton <- buildButton "search"

  treebankDlSpan <- UI.span
  element treebankDlSpan # set html (unlines [
      "<a href=\"" ++ uri1 ++ "\" download>save L1 as CoNNL-U</a>"
    , "<a href=\"" ++ uri2 ++ "\" download>save L2 as CoNNL-U</a>"])
  element treebankDlSpan # set UI.style [("margin","1%")]
  element treebankDlSpan # set UI.class_ "unselectable"
  hide treebankDlSpan

  tsvDlSpan <- UI.span
  element tsvDlSpan # set html ("<a href=\"" ++ uri ++ "\" download>save as TSV</a>")
  element tsvDlSpan # set UI.style [("margin","1%")]
  element tsvDlSpan # set UI.class_ "unselectable"
  hide tsvDlSpan

  replacementInput <- buildTextInput 
                        "additional replacement rule (optional)"
                        "replacement"
  element replacementInput # set UI.style [("width","99.2%")]

  modeSpan <- string "Mode: "
  element modeSpan # set UI.class_ "unselectable"
  
  textMode <- buildMode "text" True
  conlluMode <- buildMode "CoNNL-U" False
  treeMode <- buildMode "tree" False

  nHitsSpan <- string "0 hits"
  element nHitsSpan # set UI.class_ "unselectable"
  hide nHitsSpan

  getBody window #+ [
                element globalStyle
              , element l1Input
              , element l2Input
              , element queryInput 
              , element replacementInput
              , element modeSpan
              , element textMode
              , element conlluMode
              , element treeMode
              , element searchButton
              , element nHitsSpan
              , element treebankDlSpan
              , element tsvDlSpan
                ] 
  
  on UI.click searchButton $ const $ do
    hide treebankDlSpan
    hide tsvDlSpan
    hide nHitsSpan

    l1Path <- get value l1Input 
    l2Path <- get value l2Input
    
    queryTxt <- get value queryInput
    replacementTxt <- get value replacementInput
    
    textModeButton <- getElementById window "text"
    conlluModeButton <- getElementById window "CoNNL-U"
    treeModeButton <- getElementById window "tree"

    textModeValue <- get UI.checked (fromJust $ textModeButton)
    conlluModeValue <- get UI.checked (fromJust $ conlluModeButton)
    treeModeValue <- get UI.checked (fromJust $ treeModeButton)

    let mode = case (textModeValue, conlluModeValue, treeModeValue) of
                (True,_,_) -> TextMode 
                (_,True,_) -> CoNNLUMode
                (_,_,True) -> TreeMode

    l1Exists <- liftIO $ doesFileExist l1Path
    l2Exists <- liftIO $ doesFileExist l2Path
    let patterns = if null queryTxt
                  then [(DEPREL_ "root",DEPREL_ "root")]
                  else parseQuery fieldVals queryTxt
    let mreplacement = if null replacementTxt 
                        then Just $ CHANGES [] 
                        else readMaybe replacementTxt
    let validReplacement = isJust mreplacement
    let validQuery = not $ null patterns

    if and [l1Exists, l2Exists, validReplacement, validQuery]
      then do
        markRight l1Input
        markRight l2Input
        markRight queryInput
        markRight replacementInput
        l1Text <- liftIO $ readFile l1Path
        l2Text <- liftIO $ readFile l2Path 
        let l1Sents = (parseUDText . unpack . decodeUtf8) l1Text
        let l2Sents = (parseUDText . unpack . decodeUtf8) l2Text

        let treebank = l1Sents `zip` l2Sents 
        let alignments = map align treebank
        -- true bilingual matches
        let bimatches = treebank `zip` map (match patterns) alignments
        -- all matches (add L2-only with dummy alignments)
        let matches = 
              map  (
                (\bms@((s1,s2),ms) -> 
                  let pattern = patterns !! 0 
                  in if isL2only $ pattern 
                    then ((s1,s2), ms ++ ((repeat $ dummyUDTree) `zip` filter 
                      (\t -> not $ t `elem` (map snd ms)) 
                      (matchesUDPattern (snd $ (pattern)) (udSentence2tree s2))))
                    else bms) )
                bimatches
        let matches' = 
              map 
                (\(s,es) -> 
                  (s,map (applyReplacement (fromJust $ mreplacement)) es)) 
                (filter (\(_,ms) -> not $ null ms) matches)
        let (l1Col,l2Col) = unzip $ concatMap 
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
        destroyTables window
        table <- buildTable window l1Col l2Col mode
        liftIO $ writeFile path (encodeUtf8 $ pack $ unlines $ map 
          (\(l1,l2) -> l1 ++ "\t" ++ l2)
          ((map rmBold l1Col) `zip` (map rmBold l2Col)))
        liftIO $ writeFile path1 (encodeUtf8 $ pack $ unlines l1Col)
        liftIO $ writeFile path2 (encodeUtf8 $ pack $ unlines l2Col)
        -- tree mode (currently) behaves as CoNNL-U mode in terms of export
        if mode == TextMode then unhide tsvDlSpan else unhide treebankDlSpan
        element nHitsSpan # set text ((show $ length l1Col) ++ " hits")
        unhide nHitsSpan
        getBody window #+ [element table, element nHitsSpan]
      else do
        if l1Exists then markRight l1Input else markWrong l1Input
        if l2Exists then markRight l2Input else markWrong l2Input
        if validQuery then markRight queryInput else markWrong queryInput
        if validReplacement 
          then markRight replacementInput 
          else markWrong replacementInput
  where 
    rmBold s = replace "</b>" "" (replace "<b>" "" s)
    applyReplacement r (e1,e2) = 
      (fst $ replacementsWithUDPattern r e1,
       fst $ replacementsWithUDPattern r e2)

type Placeholder = String
type Class = String

buildTextInput :: Placeholder -> Class -> UI Element
buildTextInput p c = do
  input <- UI.input
  element input # set (UI.attr "placeholder") p
  element input # set (UI.attr "class") c
  element input # set UI.class_ "unselectable"
  markRight input
  return input

buildButton :: String -> UI Element
buildButton txt = do  
  button <- UI.button
  element button # set UI.text txt
  element button # set UI.style [("margin","1%")]
  element button # set UI.id_ txt
  element button # set UI.class_ "unselectable"
  return button

buildMode :: String -> Bool -> UI Element
buildMode mode checked = do
  span <- UI.span
  radioButton <- UI.input
  label <- UI.label
  element radioButton # set UI.type_ "radio"
  element radioButton # set UI.name "mode"
  element radioButton # set UI.id_ mode
  element radioButton # set UI.checked checked
  element label # set UI.type_ "label"
  element label # set UI.text mode
  element label # set UI.for mode
  element span # set children [radioButton, label]
  element span # set UI.class_ "unselectable"
  return span
      
buildTable :: Window -> [String] -> [String] -> Mode -> UI Element
buildTable window l1Data l2Data mode = do 
  cells <- mapM (mapM 
                  (return . (\htmlText -> do
                    div <- UI.div
                    element div # set html htmlText
                    element div # set UI.style [
                        ("text-align", "left")
                      , ("padding", "8px")
                      , ("white-space", "pre-wrap")
                      , ("overflow", "auto")
                      , ("font-family", if mode == CoNNLUMode 
                                          then "monospace, monospace"
                                          else "inherit")
                      ]
                    return div)))
            (zipWith (\s1 s2 -> [s1,s2]) l1Data l2Data)
  table <- UI.grid cells
  element table # set UI.style [("width","100%"), ("table-layout","fixed")]
  return table

destroyTables :: Window -> UI ()
destroyTables window = do
  tables <- getElementsByClassName window "table"
  mapM_ delete tables 

--  TODO: refactor (should be UI.Element -> UI Element)

markWrong :: Element -> UI Element
markWrong input = element input # set UI.style [("background-color", red)]
  where red = "#FF7E7E"

markRight :: Element -> UI Element
markRight input = element input # set UI.style [("background-color","white")]

hide :: Element -> UI Element
hide el = element el # set UI.style [("visibility", "hidden")]

unhide :: Element -> UI Element
unhide el = element el # set UI.style [("visibility", "visible")]