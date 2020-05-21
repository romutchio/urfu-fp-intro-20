{-# LANGUAGE DeriveAnyClass #-}

module Utils where

import Data.Text as T
import Control.Monad.Except
import Servant.Server
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import Network.HTTP.Types (hContentType)

throwJSONError :: (MonadError ServerError m, ToJSON a) => ServerError -> a -> m b
throwJSONError err json = throwError $ err
  { errBody = encode json
  , errHeaders = [ jsonHeader ]
  }
  where
    jsonHeader =
      ( hContentType
      , "application/json;charset=utf-8" )

data JSONError = JSONError
  { error :: Text
  } deriving (Generic, ToJSON)

toServerError
  :: String
  -> ServerError
toServerError e = ServerError
  { errHTTPCode = 500
  , errReasonPhrase = ""
  , errBody = encode $ JSONError $ T.pack e
  , errHeaders = [("content-type", "application/json")] }