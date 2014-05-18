{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Data.IHO.S52.YesodAppData where

import Yesod
import Data.IHO.S52.Types
import Prelude

data PreslibSub = PreslibSub {
      preslib_lib :: Library
} 

mkYesodSubData "PreslibSub" [parseRoutes|
  /                       PresLibHtmlR GET
  /LBID			  PresLibIdHtmlR GET
  /COLS			  PresLibColsListHtmlR GET
  /COLS/#String           PresLibColsHtmlR GET
  /COLS/#String/style.css PresLibColsCssR GET
|]


mkMessage "PreslibSub" "messages_preslib" "en"

