{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Data.IHO.Preslib.Data where

import Yesod
import Data.DAI.Types
import Prelude

data PreslibSub = PreslibSub {
      preslib_lib :: Library
} 

mkYesodSubData "PreslibSub" [parseRoutes|
  /LBID			  PresLibIdHtmlR GET
  /COLS			  PresLibColsListHtmlR GET
  /COLS/#String           PresLibColsHtmlR GET
  /COLS/#String/style.css PresLibColsCssR GET
|]


mkMessage "PreslibSub" "messages_preslib" "en"

