{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad             ((>=>))
import qualified Data.ByteString.Lazy      as BS
import           Data.Maybe                (catMaybes)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import           Network.HTTP.Types        (methodGet, methodPost, status200,
                                            status404, status500)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (Application, Request, Response,
                                            pathInfo, requestMethod,
                                            responseLBS)
import           Network.Wai.Handler.Warp  (run)

data HttpContext' = HttpContext' { req :: Request, res :: Maybe Response }

type HttpContext = Maybe HttpContext'

data Config = Config { cfgPort :: Int }

type WebPart = HttpContext' -> HttpContext

initialContext :: Request -> HttpContext'
initialContext r = HttpContext' r Nothing

isGet :: HttpContext' -> Bool
isGet ctx = requestMethod (req ctx) == methodGet

isPost :: HttpContext' -> Bool
isPost ctx = requestMethod (req ctx) == methodPost

pathMatches :: T.Text -> HttpContext' -> Bool
pathMatches path ctx = pathInfo (req ctx) == testedPath
  where testedPath = filter (/= "") (T.splitOn "/" path)

choose :: [HttpContext' -> HttpContext] -> HttpContext' -> HttpContext
choose rs ctx = do
  let matches = fmap ($ ctx) rs
  case catMaybes matches of
    []      -> Nothing
    [match] -> Just match
    _       -> error "Multiple route matches found!"

mustSatisfy :: (a -> Bool) -> a -> Maybe a
mustSatisfy p a = if p a then Just a else Nothing

get :: HttpContext' -> HttpContext
get = mustSatisfy isGet

post :: HttpContext' -> HttpContext
post = mustSatisfy isPost

path :: T.Text -> HttpContext' -> HttpContext
path s = mustSatisfy (pathMatches s)

ok :: BS.ByteString -> HttpContext' -> HttpContext
ok r ctx = Just (ctx { res = Just $ textResponse r})

textResponse :: BS.ByteString -> Response
textResponse t = responseLBS status200 [(hContentType, "text/plain")] t

testApp :: WebPart
testApp = choose
  [ get >=> choose
    [ path "/" >=> ok "Homepage GET!"
    , path "/hello" >=> ok "Hello GET"
    , path "/goodbye" >=> ok "Goodbye GET" ]
  , post >=> choose
    [ path "/hello" >=> ok "Hello POST"
    , path "/goodbye" >=> ok "Goodbye POST" ]
  ]

startWebServer :: Config -> WebPart -> IO ()
startWebServer cfg wp = do
  let port = cfgPort cfg
  run port (toWai wp)

toWai :: WebPart -> Application
toWai wp r f = case wp (initialContext r) of
  Just ctx -> case res ctx of
    Just response -> f response
    Nothing -> f $ responseLBS status500 [(hContentType, "text/plain")] "Internal Error."
  Nothing -> f $ responseLBS status404 [(hContentType, "text/plain")] "Not Found!"


main :: IO ()
main = startWebServer (Config 3000) testApp
