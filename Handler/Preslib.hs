{-# LANGUAGE TupleSections, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Handler.Preslib where
import qualified Data.Text as Text
import Import
import Data.DAI.Types
import Data.DAI.CSS
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

getPresLibColsR :: String -> Handler Html
getPresLibColsR c = do
  plib <- fmap preslib getYesod
  case (findCOLS plib $ Text.pack c) of
    Nothing -> notFound
    Just cols -> 
        let cols_ks = Map.keys es
            es = cols_entries cols
        in defaultLayout $ do                      
                      addStylesheet $ PresLibColsCssR c
                      setTitle $ toHtml $ concat ["Colour Schema ", c]
                      [whamlet|     
                                <h1>Colour Schema #{c}
                                <table style="border-width:2px; border-style: solid;">
                                  $forall colour <- cols_ks
                                    <tr>
                                       <td>#{colour}
                                       <td .#{colour}_bg style="width: 80px">&nbsp;
                                       <td .#{colour}>#{snd $ fromJust $ Map.lookup colour es}
                       |]
      

getPresLibColsListR :: Handler Html
getPresLibColsListR = do
  cols <- fmap (lib_cols . preslib) getYesod
  let ks = map cols_ctus cols
  defaultLayout $ do
    setTitle "COLS"
    [whamlet| <h1>Defined Colour Schemas</h1>
              <ul>
              $forall k <- ks
                  <li><a href=@{PresLibColsR $ Text.unpack k}>#{k}</a></li>
     |]

getPresLibColsCssR :: String -> Handler TypedContent
getPresLibColsCssR c = do
  plib <- fmap preslib getYesod
  case (findCOLS plib $ Text.pack c) of
    Nothing -> notFound
    Just cols -> respond "text/css" $ cssColourMap . cols_entries $ cols

findCOLS :: Library -> Text -> Maybe (Record ColourTable)
findCOLS lib tn =
    let cols = lib_cols lib
    in find (\c -> cols_ctus c == tn) cols
                    

