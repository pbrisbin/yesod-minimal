{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Minimal where

import Yesod
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)

data Minimal = Minimal

mkYesod "Minimal" [parseRoutes|
    / RootR GET POST
|]

type Form x = Html -> MForm Minimal Minimal (FormResult x, Widget)

instance Yesod Minimal where 
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

instance RenderMessage Minimal FormMessage where
    renderMessage _ _ = defaultFormMessage

data Fruit = Apple | Orange | Pear deriving (Eq, Ord, Read, Show)

data TheForm = TheForm
    { formText  :: Text
    , formInt   :: Int
    , formFruit :: Fruit
    }

theForm :: Form TheForm
theForm = renderDivs $ TheForm
    <$> areq textField   "Some text"   Nothing
    <*> areq intField    "Some number" Nothing
    <*> areq selectFruit "Some fruit"  Nothing

    where
        selectFruit :: Field Minimal Minimal Fruit
        selectFruit = selectField $ return $ mkOptionList [ Option "Apple" Apple "apple"
                                                          , Option "Orange" Orange "orange"
                                                          , Option "Pear" Pear "pear"
                                                          ]

getRootR :: Handler RepHtml
getRootR = do
    ((res, form), enctype ) <- runFormPost theForm
    defaultLayout $ do
        setTitle "My title"

        case res of
            FormSuccess f -> [whamlet|
                                <p>You've posted a form!
                                <p>the text was #{formText f}
                                <p>the number was #{formInt f}
                                <p>the fruit was #{show $ formFruit f}
                                |]

            _ -> [whamlet|
                    <p>Hello world!
                    <form enctype="#{enctype}" method="post">
                        ^{form}
                        <input type="submit">
                    |]


postRootR :: Handler RepHtml
postRootR = getRootR

main :: IO ()
main = run 3000 =<< toWaiApp Minimal
