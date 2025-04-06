{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Control.Exception (Exception (..))
import Control.Monad
import Data.String (fromString)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Network.HTTP.Types (status302)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get "/" $ text "hello haskell!"
  get "/cat/:p" $ do
    v <- pathParam "p"
    html $ mconcat ["<h1>", v, "</h1>"]
