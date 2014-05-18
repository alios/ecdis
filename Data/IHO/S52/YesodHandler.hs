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

module Data.IHO.S52.YesodHandler
    ( module Data.IHO.S52.YesodAppData
    , mkPreslibSub
    ) where

import Prelude

import Data.IHO.S52.YesodAppData
import Yesod
import qualified Data.Text as Text
import Data.IHO.S52.Types
import Data.IHO.S52.CSS
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.Text (Text)
import Control.Monad.Logger 
import Network.HTTP.Types.Status
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import Blaze.ByteString.Builder.Internal.Types (Builder)
import Data.Time.Format as Time
import Data.Time.Clock
import System.Locale
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
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
    Left err -> fail err
    Right res ->
        do runStdoutLoggingT ($(logInfo) (lbid_comt . lib_lbid $ res))
           return res
    where readLibFile :: FilePath -> IO (Either String Library)
          readLibFile fp = do fmap (parseOnly parseLibrary) $ T.readFile fp


type PreslibHandler t = (Yesod master)=> HandlerT PreslibSub (HandlerT master IO) t

instance Yesod master => YesodSubDispatch PreslibSub (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPreslibSub)


--
-- Handlers
--
getPresLibHtmlR :: PreslibHandler Html
getPresLibHtmlR = do
  lib <- fmap preslib_lib getYesod
  i18n <- getMessageRender
  setModifiedCompileTime
  defaultLayoutSub $ do
    setTitle . toHtml . i18n $ MsgPreslibId
    [whamlet|
     <h1>_{MsgPreslibId}
     #{lbid_comt $ lib_lbid lib}
     <br />
     <a href="@{PresLibIdHtmlR}">
        _{MsgPreslibClickMore}
     <h2>_{MsgPreslibCOLS} (#{length $ lib_cols lib} _{MsgPreslibEntries}) 
     <a href="@{PresLibColsListHtmlR}">
        _{MsgPreslibClickMore}
     <h2>_{MsgPreslibLUPT} (#{length $ lib_lupt lib} _{MsgPreslibEntries})
     <a href="@{PresLibLuptListHtmlR}">
        _{MsgPreslibClickMore}

     <h2>_{MsgPreslibLNST} (#{length $ lib_lnst lib} _{MsgPreslibEntries})
     <h2>_{MsgPreslibSYMB} (#{length $ lib_symb lib} _{MsgPreslibEntries})
     <h2>_{MsgPreslibPATT} (#{length $ lib_patt lib} _{MsgPreslibEntries})
     |]


getPresLibIdHtmlR :: PreslibHandler Html 
getPresLibIdHtmlR = do
  lid <- fmap (lib_lbid . preslib_lib) getYesod
  i18n <- getMessageRender
  setModifiedCompileTime
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
  defaultLayoutSub $ do
    setTitle . toHtml . i18n $ MsgPreslibId
    [whamlet|
     <h1>_{MsgPreslibId}
     <table style="border-width:2px; border-style: solid;">
                $forall (k, v) <- id_fields
                            <tr><td>#{k}</td><td>#{v}</td
     |]



getPresLibLuptListHtmlR :: PreslibHandler Html
getPresLibLuptListHtmlR = do 
  lib <- fmap preslib_lib getYesod
  i18n <- getMessageRender
  setModifiedCompileTime
  let lupt = lib_lupt lib
  defaultLayoutSub $ do
    setTitle . toHtml . i18n $ MsgPreslibLUPT
    [whamlet|
     <h2>_{MsgPreslibLUPT} 
     <p>
       (#{length lupt} _{MsgPreslibEntries})
     <table border="1" style="border-width:2px; border-style: solid;">
       <tr>
         <th>OBCL
         <th>FTYP
         <th>ATTC
         <th>DPRI
         <th>RPRI
         <th>TNAM
         <th>DISC
         <th>LUCM
       $forall r <- lupt
         <tr>
           <td>#{lupt_obcl r}
           <td>#{lupt_ftyp r}
           <td>
             <ul>
               $forall (k,v) <- lupt_attc r
                 <li>
                   <b>#{k}: 
                   <span>#{v}
           <td>#{toInteger $ lupt_dpri r}                        
           <td>#{lupt_rpri r}                        
           <td>#{lupt_tnam r}
           <td>#{lupt_disc r}
           <td>#{lupt_lucm r}
            |]


getPresLibColsHtmlR :: String -> PreslibHandler Html
getPresLibColsHtmlR c = do
  plib <- fmap (preslib_lib) getYesod
  setModifiedCompileTime
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
    Just cols -> return $ toTypedContent cols


css_builder :: Record ColourTable -> Builder
css_builder cols = B.fromLazyText $ cssColourMap . cols_entries $ cols

instance ToContent (Record ColourTable) where
    toContent cols = ContentBuilder (css_builder cols) Nothing

instance ToTypedContent (Record ColourTable) where
    toTypedContent cols = TypedContent typeCss $ toContent cols
--
-- Helpers
--
setModifiedCompileTime :: PreslibHandler ()
setModifiedCompileTime = do
  ct <- fmap (lbid_compileTime . lib_lbid . preslib_lib) getYesod  
  hdr <-  lookupHeader "If-Modified-Since"
  ($(logInfo) "loading presentation library")  
  case hdr of
    Just hv -> 
        let t2 = parseUTCTime . E.decodeUtf8 $ hv 
        in if (ct <= t2) 
           then sendResponseStatus notModified304 ()
           else return ()
    Nothing -> do return ()
  neverExpires
  addHeader "Last-Modified" $ formatRFC1123 ct

findCOLS :: Library -> Text -> Maybe (Record ColourTable)
findCOLS lib tn =
    let cols = lib_cols lib
    in find (\c -> cols_ctus c == tn) cols

showText :: forall a. Show a => a -> Text
showText = Text.pack . show

parseUTCTime :: Text -> UTCTime
parseUTCTime i =
    case (Time.parseTime defaultTimeLocale "%a, %d %b %Y %T %Z" $ T.unpack i) of
      Nothing -> error $ "unable to parse date: " ++ T.unpack i
      Just d -> d


