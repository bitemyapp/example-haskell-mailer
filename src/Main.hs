{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)
import Network.Api.Postmark (Email(..), PostmarkError, PostmarkResponse, PostmarkSettings(..))
import qualified Network.Api.Postmark as Post
import Web.Scotty

postmarkSettings :: Text -> PostmarkSettings
postmarkSettings apiToken =
  PostmarkSettings "https://api.postmarkapp.com" apiToken

defaultEmail :: Text
             -> Text
             -> [Text]
             -> Email
defaultEmail subject html addressees =
   Post.defaultEmail {
          -- replace with your sender signature'd email
          emailFrom = "support@yoursite.com"
        , emailTo = addressees -- ["your@email.com"]
        , emailSubject = subject -- "demo, yes it really is a demo"
        , emailTag = Nothing
        , emailHtml = Just html
        , emailReplyTo = "support@yoursite.com"
        }

sendEmail :: PostmarkSettings
          -> Email
          -> IO (PostmarkResponse PostmarkError Post.Sent)
sendEmail pmarkSettings em = do
  Post.request pmarkSettings $ (Post.email em)

supportRequest :: Text -> IO ()
supportRequest msg = do
  -- Replace the postmark token with your own, be careful
  -- not to check it into source control! Might consider
  -- getting this data from an environment variable as well.
  let settings = postmarkSettings "POSTMARK_API_TEST"
  response <- sendEmail settings (defaultEmail "This is a test" msg ["your@email.com"])
  print response

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ mconcat ["<h1>", "Chris", "</h1>"]
  post "/support/email/" $ do
    sender <- param "sender"
    msg <- param "msg"
    let supportMsg :: Text
        supportMsg = "You got a support request from: " <> sender <>
                     "\n\n<br><br>Their support request was: " <> msg
    liftIO $ supportRequest supportMsg
