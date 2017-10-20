{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad             ((>=>))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
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

data HttpContext = HttpContext { req :: Request, res :: Maybe Response }

type HuaveM = MaybeT IO HttpContext

data Config = Config { cfgPort :: Int }

type WebPart = HttpContext -> HuaveM

initialContext :: Request -> HttpContext
initialContext r = HttpContext r Nothing

isGet :: HttpContext -> Bool
isGet ctx = requestMethod (req ctx) == methodGet

isPost :: HttpContext -> Bool
isPost ctx = requestMethod (req ctx) == methodPost

pathMatches :: T.Text -> HttpContext -> Bool
pathMatches path ctx = pathInfo (req ctx) == testedPath
  where testedPath = filter (/= "") (T.splitOn "/" path)

-- Picks the first match from the list of routes
choose :: [HttpContext -> HuaveM] -> HttpContext -> HuaveM
choose [] _ = MaybeT $ return Nothing
choose (r:rs) ctx = do
  maybeMatch <- liftIO $ runMaybeT $ r ctx
  maybe (choose rs ctx) (return . id) maybeMatch

mustSatisfy :: (a -> Bool) -> a -> Maybe a
mustSatisfy p a = if p a then Just a else Nothing

get :: HttpContext -> HuaveM
get = MaybeT . return . mustSatisfy isGet

post :: HttpContext -> HuaveM
post = MaybeT . return . mustSatisfy isPost

path :: T.Text -> HttpContext -> HuaveM
path s = MaybeT . return . mustSatisfy (pathMatches s)

ok :: BS.ByteString -> HttpContext -> HuaveM
ok r ctx = MaybeT $ return $ Just (ctx { res = Just $ textResponse r})

textResponse :: BS.ByteString -> Response
textResponse t = responseLBS status200 [(hContentType, "text/plain")] t

testApp :: WebPart
testApp = choose
  [ get >=> choose
    [ path "/" >=> ok "Homepage GET!"
    , path "/hello" >=> ok "Hello GET"
    -- this route is simply ignored:
    , path "/hello" >=> ok "Hello from duplicate GET"
    , path "/goodbye" >=> ok "Goodbye GET" ]
  , post >=> choose
    [ path "/hello" >=> ok "Hello POST"
    , path "/goodbye" >=> ok "Goodbye POST" ]
  ]

startWebServer :: Config -> WebPart -> IO ()
startWebServer cfg wp = do
  let port = cfgPort cfg
  app <- toWai wp
  run port app

toWai :: WebPart -> IO (Application)
toWai wp = return $ \r f -> do
  result <- runMaybeT $ wp (initialContext r)
  case result of
    Just ctx -> case res ctx of
      Just response -> f response
      Nothing -> f $ responseLBS status500 [(hContentType, "text/plain")] "Internal Error."
    Nothing -> f $ responseLBS status404 [(hContentType, "text/plain")] "Not Found!"

main :: IO ()
main = startWebServer (Config 3000) testApp
