{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Amazonka hiding (length)
import qualified Amazonka as AWS
import Amazonka.S3 as S3
import Control.Exception (Exception (..))
import Control.Lens (set, (<&>), (^.))
import Control.Lens.Combinators (view)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSSU
import qualified Data.Conduit.Binary as CB
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.SecureMem
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import Data.Typeable (Typeable)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified Network.Wai as Wai
import Network.Wai.Middleware.HealthCheckEndpoint
import Network.Wai.Middleware.HttpAuth
import Network.Wai.Middleware.RequestLogger
import System.Directory (canonicalizePath, doesDirectoryExist, getDirectoryContents)
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))
import System.IO
import qualified System.Random as Random
import Web.Scotty
import qualified Web.Scotty as Scotty

secureS :: String -> SecureMem
secureS s = secureMemFromByteString $ BS.toStrict $ BLU.fromString s

data CatException
  = IOError
  deriving (Show, Typeable)

instance Exception CatException

data Config = Config
  { -- Digital Ocean Bucket
    appName :: Text.Text,
    appService :: AWS.Service,
    appUser :: BS.ByteString,
    appPassword :: SecureMem
  }

-- ONLY use over HTTPS
basicCatAuth :: Config -> Wai.Middleware
basicCatAuth config =
  basicAuth
    (\u p -> return $ u == BS.toStrict (appUser config) && secureMemFromByteString p == (appPassword config))
    "Cat Upload Service"

-- Create a custom middleware that only applies to specific paths
pathSpecificAuth :: Config -> String -> Wai.Middleware
pathSpecificAuth config targetPath app req respond =
  if Wai.rawPathInfo req == BSSU.fromString targetPath
    then (basicCatAuth config) app req respond
    else app req respond

main :: IO ()
main = do
  -- For regino use AWS_REGION env
  doBucketEndpointStr <- getEnv "BUCKET_ENDPOINT"
  apiPassword <- getEnv "API_PASS"
  apiUser <- getEnv "API_USER"
  bucketName <- fromMaybe "cat-rest" <$> lookupEnv "APP_NAME"
  let digitalOcean = AWS.setEndpoint True (encodeUtf8 $ Text.pack doBucketEndpointStr) 443 S3.defaultService
  let config = Config {appName = Text.pack bucketName, appService = digitalOcean, appUser = BSC.pack apiUser, appPassword = secureS apiPassword}
  runServer config

runServer :: Config -> IO ()
runServer config = scotty 3000 $ do
  middleware logStdoutDev
  middleware healthCheck
  -- Apply path-specific middleware
  middleware (pathSpecificAuth config "/upload")

  get "/" $ do
    contents <- liftIO $ Text.readFile "cats/cat2.txt"
    text $ LazyText.fromStrict contents

  get "/cat/:id" $ do
    id <- pathParam "id"
    cat <- liftIO $ getCat config (ObjectKey id)
    -- setHeader "Content-Type" "image/jpeg" -- Adjust as needed for your image type
    Scotty.raw cat

  get "/catscii" $ do
    cats1 <- liftIO $ getAbsDirectoryContents "./cats"
    cats2 <- liftIO $ getAbsDirectoryContents "./external-cats"
    let cats = filter (`notElem` [".", ".."]) (cats1 ++ cats2)
    cats <- liftIO $ filterM (\path -> not <$> doesDirectoryExist path) cats
    chosen_one <- liftIO $ randomChoice cats
    chosen_one <- liftIO $ readFile chosen_one
    Scotty.text $ LazyText.pack chosen_one

  post "/upload" $ do
    b <- Scotty.body
    id <- liftIO nextRandom
    liftIO $ putCat config (ObjectKey $ UUID.toText id) b
    Scotty.json $ object ["id" .= UUID.toText id]

randomChoice :: [a] -> IO a
randomChoice xs = do
  idx <- Random.randomRIO (0, length xs - 1)
  return (xs !! idx)

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir =
  getDirectoryContents dir >>= mapM (canonicalizePath . (dir </>))

getCat ::
  Config ->
  ObjectKey ->
  IO BS.ByteString
getCat c key = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> (AWS.configureService (appService c) . set #logger lgr)
  runResourceT $ do
    resp <- send env $ newGetObject (BucketName $ appName c) key
    body <- view #body resp `sinkBody` CB.sinkLbs
    liftIO $
      say $
        "Successfully retrieved: "
          <> BucketName (appName c) ^. _BucketName
          <> " - "
          <> key ^. _ObjectKey
    return body

putCat ::
  Config ->
  -- | The destination object key.
  ObjectKey ->
  -- | The chunk size to send env.
  BS.ByteString ->
  IO ()
putCat c k cat = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> (AWS.configureService (appService c) . set #logger lgr)
  runResourceT $ do
    void . send env $ newPutObject (BucketName $ appName c) k (toBody cat)
    say $
      "Cat Successfully Uploaded:"
        <> " to "
        <> BucketName (appName c) ^. _BucketName
        <> " - "
        <> k ^. _ObjectKey

say :: (MonadIO m) => Text.Text -> m ()
say = liftIO . Text.putStrLn
