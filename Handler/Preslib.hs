{-# LANGUAGE TupleSections, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, Rank2Types #-}
module Handler.Preslib where
import qualified Data.Text as Text
import Import
import Data.DAI.Types
import Data.DAI.CSS
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

showText :: forall a. Show a => a -> Text
showText = Text.pack . show

getPresLibIdHtmlR :: Handler Html
getPresLibIdHtmlR = do
  lid <- fmap (lib_id . preslib) getYesod
  i18n <- getMessageRender
  let id_fields =
          [ (i18n MsgPreslibEXPP, lbid_expp lid) 
          , (i18n MsgPreslibPTYP, lbid_ptyp lid)
          , (i18n MsgPreslibESID, showText $ lbid_esid lid) 
          , (i18n MsgPreslibEDTN, showText $ lbid_edtn lid) 
          , (i18n MsgPreslibCODT, showText $ lbid_codt lid) 
          , (i18n MsgPreslibCOTI, showText $ lbid_coti lid) 
          , (i18n MsgPreslibVRDT, showText $ lbid_vrdt lid) 
          , (i18n MsgPreslibPROF, lbid_prof lid) 
          , (i18n MsgPreslibOCDT, showText $ lbid_ocdt lid) 
          , (i18n MsgPreslibCOMT, lbid_comt lid) 
          ]
  defaultLayout $ do
    [whamlet|
     <h1>_{MsgPreslibId}
     <table style="border-width:2px; border-style: solid;">
                $forall (k, v) <- id_fields
                            <tr><td>#{k}</td><td>#{v}</td
            |]

getPresLibColsHtmlR :: String -> Handler Html
getPresLibColsHtmlR c = do
  plib <- fmap preslib getYesod
  case (findCOLS plib $ Text.pack c) of
    Nothing -> notFound
    Just cols -> 
        let cols_ks = Map.keys es
            es = cols_entries cols
        in defaultLayout $ do                      
                      addStylesheet $ PresLibColsCssR c
                      i18n <- getMessageRender
                      setTitle $ toHtml $ concat [Text.unpack $ i18n MsgColorDefinition, c]
                      [whamlet|     
                                <h1>_{MsgColorDefinition} #{c}
                                <table style="border-width:2px; border-style: solid;">
                                  $forall colour <- cols_ks
                                    <tr>
                                       <td>#{colour}
                                       <td .#{colour}_bg style="width: 80px">&nbsp;
                                       <td .#{colour}>#{snd $ fromJust $ Map.lookup colour es}
                       |]
      

getPresLibColsListHtmlR :: Handler Html
getPresLibColsListHtmlR = do
  cols <- fmap (lib_cols . preslib) getYesod
  let ks = map cols_ctus cols
  defaultLayout $ do
    setTitle "COLS"
    [whamlet| <h1>_{MsgColorDefinition}</h1>
              <ul>
              $forall k <- ks
                  <li><a href=@{PresLibColsHtmlR $ Text.unpack k}>#{k}</a></li>
     |]

getPresLibColsCssR :: String -> Handler TypedContent
getPresLibColsCssR c = do
  plib <- fmap preslib getYesod
  case (findCOLS plib $ Text.pack c) of
    Nothing -> notFound
    Just cols -> respond "text/css" $ cssColourMap . cols_entries $ cols

--
-- Helpers
--

findCOLS :: Library -> Text -> Maybe (Record ColourTable)
findCOLS lib tn =
    let cols = lib_cols lib
    in find (\c -> cols_ctus c == tn) cols
                    

