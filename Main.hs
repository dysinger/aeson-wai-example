{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson                     as DA
import qualified Data.Text                      as DT
import qualified Network.HTTP.Types             as NHT
import qualified Network.Wai                    as NW
import qualified Network.Wai.Application.Static as NWAS
import qualified Network.Wai.Handler.Warp       as NWHW

data User = User { name :: DT.Text, email :: DT.Text }
          deriving (Show, Read, Eq)

instance DA.ToJSON User where
  toJSON x = DA.object [ "name" DA..= (name x), "email" DA..= (email x) ]

resource :: NHT.StdMethod -> DT.Text -> NW.Application
resource NHT.GET _uuid _req =
  -- we are ignoring the uuid but you can see how you would use it
  return $ NW.responseLBS NHT.statusOK headers json
  where user = User { name = "Tim Dysinger", email = "tim@dysinger.net" }
        json = DA.encode user
        headers = [("Content-Type", "application/json")]
resource _       _     _    =
  -- we only accept GET requests right now
  return $ NW.responseLBS NHT.statusNotAllowed [("Accept", "GET")] ""

dispatch :: NW.Middleware
dispatch app req =
  case path req of
    -- handle the route /api/v1/resource/:uuid
    ("api":"v1":"resource":uuid:_) -> resource (method req) uuid req
    -- otherwise pass the request to the next app
    _                              -> app req
  where path :: NW.Request -> [DT.Text]
        path req' = filter (\p -> p /= "") $ map DT.toLower $ NW.pathInfo req'
        method :: NW.Request -> NHT.StdMethod
        method req' = case NHT.parseMethod $ NW.requestMethod req' of
          Right m -> m
          Left  _ -> NHT.GET

main :: IO ()
main = NWHW.run 8080 $
       -- pass the request to dispatch
       dispatch $
       -- but fall through to serving static content if nothing else
       NWAS.staticApp NWAS.defaultFileServerSettings
