{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Minimal where

import Yesod
import Network.Wai.Handler.Warp (run)

data Minimal = Minimal

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

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle "My title"
    
    [whamlet|<p>Hello world|]

main :: IO ()
main = run 3000 =<< toWaiApp Minimal
