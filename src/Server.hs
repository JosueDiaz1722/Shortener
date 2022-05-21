{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
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
import ApiType (URL)




-- type UrlAPI = "urls" :> Get '[JSON] [Url]

type API = Get '[HTML] Homepage
type Homepage = H.Html

api :: Proxy API
api = Proxy

server :: Server API
server = return myHome

myHome :: Homepage
myHome = 
    H.html $
        H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"
            H.table $
              forM_(url1) $ \(url) ->
                H.tr $ do
                  H.td (H.text url)
app :: Application
app = serve api server

data Url = Url
  { url :: String
  }



url1 :: [Text] 
url1 =  ["a", "b"]
-- data Url = Url
--   { url :: String
--   } deriving (Eq, Show, Generic)

-- instance ToJSON Url

-- server :: Server UrlAPI
-- server = return urls1

-- urlsAPI :: Proxy UrlAPI
-- urlsAPI = Proxy

-- app :: Application
-- app = serve urlsAPI server

