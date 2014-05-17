{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IHO.Preslib 
    ( module Data.IHO.Preslib.Data
    , mkPreslibSub
    ) where

import Prelude

import Data.IHO.Preslib.Data
import Yesod

import qualified Data.Text as Text
import Data.DAI.Types
import Data.DAI.CSS
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.Text (Text)
import Control.Monad.Logger 

mkPreslibSub :: FilePath -> IO PreslibSub
mkPreslibSub fp = do
  lib <- loadPreslib fp
  return $ PreslibSub
         { preslib_lib = lib
         }

loadPreslib :: FilePath -> IO Library
loadPreslib fn = do
  runStdoutLoggingT ($(logInfo) "loading presentation library")
  lib_res <- readLibFile fn
  case lib_res of
    Fail _ ss s -> fail $ "parseFail" ++ show ss ++ s
    Partial _ -> fail "partial Parse fail"
    Done _ res -> 
        do runStdoutLoggingT ($(logInfo) (lbid_comt . lib_id $ res))
           return res
    where readLibFile :: FilePath -> IO (Result Library)
          readLibFile fp = do fmap (parse parseLibrary) $ T.readFile fp


type PreslibHandler t = (Yesod master)=> HandlerT PreslibSub (HandlerT master IO) t

instance Yesod master => YesodSubDispatch PreslibSub (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPreslibSub)


--
-- Handlers
--

getPresLibIdHtmlR :: PreslibHandler Html
getPresLibIdHtmlR = do
  lid <- fmap (lib_id . preslib_lib) getYesod
  i18n <- getMessageRender
  let id_fields =
          [ (i18n MsgPreslibEXPP, lbid_expp lid) 
          , (i18n MsgPreslibPTYP, lbid_ptyp lid)
          , (i18n MsgPreslibESID, showText $ lbid_esid lid) 
          , (i18n MsgPreslibEDTN, showText $ lbid_edtn lid) 
          , (i18n MsgPreslibCO, showText $ lbid_compileTime lid) 
          , (i18n MsgPreslibVRDT, showText $ lbid_vrdt lid) 
          , (i18n MsgPreslibPROF, lbid_prof lid) 
          , (i18n MsgPreslibOCDT, showText $ lbid_ocdt lid) 
          , (i18n MsgPreslibCOMT, lbid_comt lid) 
          ]
  setModifiedCompileTime
  defaultLayoutSub $ do
    setTitle . toHtml . i18n $ MsgPreslibId
    [whamlet|
     <h1>_{MsgPreslibId}
     <table style="border-width:2px; border-style: solid;">
                $forall (k, v) <- id_fields
                            <tr><td>#{k}</td><td>#{v}</td
     |]


getPresLibColsHtmlR :: String -> PreslibHandler Html
getPresLibColsHtmlR c = do
  plib <- fmap (preslib_lib) getYesod
  case (findCOLS plib $ Text.pack c) of
    Nothing -> notFound
    Just cols -> 
        let cols_ks = Map.keys es
            es = cols_entries cols
        in do
          setModifiedCompileTime
          defaultLayoutSub $ do                      
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


getPresLibColsListHtmlR :: PreslibHandler Html
getPresLibColsListHtmlR = do
  cols <- fmap (lib_cols . preslib_lib) getYesod
  let ks = map cols_ctus cols
  setModifiedCompileTime
  defaultLayoutSub $ do
    setTitle "COLS"
    [whamlet| <h1>_{MsgColorDefinition}</h1>
              <ul>
              $forall k <- ks
                  <li><a href=@{PresLibColsHtmlR $ Text.unpack k}>#{k}</a></li>
     |]

getPresLibColsCssR :: String -> PreslibHandler TypedContent
getPresLibColsCssR c = do
  plib <- fmap preslib_lib $ getYesod
  setModifiedCompileTime
  case (findCOLS plib $ Text.pack c) of
    Nothing -> notFound
    Just cols -> do
      respond "text/css" $ cssColourMap . cols_entries $ cols


--
-- Helpers
--
setModifiedCompileTime :: PreslibHandler ()
setModifiedCompileTime = do
  ct <- fmap (lbid_compileTime . lib_id . preslib_lib) getYesod  
  addHeader "Last-Modified" $ formatRFC1123 ct

findCOLS :: Library -> Text -> Maybe (Record ColourTable)
findCOLS lib tn =
    let cols = lib_cols lib
    in find (\c -> cols_ctus c == tn) cols

showText :: forall a. Show a => a -> Text
showText = Text.pack . show


