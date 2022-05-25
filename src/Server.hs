{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString, append)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time (UTCTime)
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html as H
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Map as M
import Data.Text
import Control.Monad (forM_)
import Web.FormUrlEncoded(FromForm(..), ToForm(..))
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,writeTVar)



-- type UrlAPI = "urls" :> Get '[JSON] [Url]

type API = Get '[HTML] Homepage
type Homepage = H.Html


myHome :: [URL_data_type] -> Homepage
myHome xs =
    H.html $
        H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url_path"
              H.input H.! A.type_ "submit" ! A.action "/urls"
            H.table $
              forM_(xs) $ \(url) ->
                  H.tr $ do
                  H.td (H.text (print_url url))
                  
app :: Application
app = serve api server


print_url :: URL_data_type -> Text
print_url =  url_path

newtype URL_type = URL T.Text deriving (Show, Generic)

data URL_data_type = URL_data_type
 { url_path    :: !T.Text
 } deriving (Eq, Show, Generic)

instance FromForm URL_data_type
instance ToJSON URL_data_type
instance FromJSON URL_data_type

type UrlsAPI =  Get '[HTML] Homepage
            :<|> "urls"
            :> Get '[JSON] [URL_data_type]
            :<|> 
            ReqBody '[FormUrlEncoded] URL_data_type
            :> Post '[HTML] Homepage
            
-- new type for just one value. 
api :: Proxy UrlsAPI
api = Proxy

server :: Server UrlsAPI
server = return (myHome urlList2) :<|> return urlList2 :<|> add_url2

urlList2 :: [URL_data_type]
urlList2 =
  [ URL_data_type "https://www.stackbuilders.com/blog/getting-started-with-haskell-projects-using-scotty/"
  , URL_data_type "https://www.stackbuilders.com"
  ]


data State = State
  { urls :: TVar [URL_type]
  }
add_url2 :: Monad m => URL_data_type -> m Homepage
add_url2 url = redirect (addUrlHandler urlList2 url)

redirect :: Monad m => [URL_data_type] -> m Homepage
redirect updateList = return $ myHome updateList

addUrlHandler :: [URL_data_type] -> URL_data_type -> [URL_data_type]
addUrlHandler a xs = do
  pure (unsafePerformIO $ putStrLn ("ads"))
  xs : a

