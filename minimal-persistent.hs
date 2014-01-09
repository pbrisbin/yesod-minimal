{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Minimal where

import Yesod
import Yesod.Default.Config hiding (loadConfig)
import Database.Persist.Sqlite
import Database.Persist.Store
import Database.Persist.Query
import Network.Wai.Handler.Warp (run)

data Minimal = Minimal
    { connPool :: PersistConfigPool SqliteConf }

mkYesod "Minimal" [parseRoutes|
    / RootR GET
|]

instance Yesod Minimal where 
    approot _            = ""
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        hamletToRepHtml [hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <meta name="description" content="my awesome site">
                    <meta name="author" content="Patrick Brisbin">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

instance YesodPersist Minimal where
    type YesodPersistBackend Minimal = SqlPersist
    runDB f = fmap connPool getYesod >>= Database.Persist.Store.runPool (undefined :: SqliteConf) f

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle "My title"
    
    [whamlet|<p>Hello world|]

main :: IO ()
main = do
    dbconf <- withYamlEnvironment "config/sqlite.yml" Development loadConfig
    withPool (dbconf :: SqliteConf) $ \p -> do
        --runPool dbconf (runMigration migrateAll) p
        run 3000 =<< toWaiApp (Minimal p)
