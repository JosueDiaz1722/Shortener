{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant.API
import Servant (Or)

-- GET /urls/?sortby={url}
type UrlsAPI = "urls"
    :> QueryParam "sortby" SortBy 
    :> Get '[JSON][URL]
--Either /urls/list-all
--Or    /list-all/urls 
type UrlsAPI2 = 
    "urls"
    :> "list-all"
    :> Get '[JSON] [URL]
    :<|> "list-all"
    :> Get '[JSON] [URL]

type UrlsAPI3 =
    "urls"
    :> ReqBody '[JSON] URL
    :> Post '[JSON] URL
    :<|> "urls"
    :> ReqBody '[JSON] URL
    :> Put '[JSON] URL

data SortBy = Url

data URL = URL{
    url :: String,
    registration_date :: UTCTime
}