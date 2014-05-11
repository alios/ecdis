{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Control.Monad.Logger 
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.DAI.Types

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Preslib

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- gzip middleware
    let gzip = Gzip.gzip Gzip.def

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation

    return $ logWare $ gzip app

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


-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager conduitManagerSettings
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    
    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    plib <- loadPreslib "config/PresLib_e4.0.0.dai"
    
    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger plib

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
