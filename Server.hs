-- This file is part of tersmu
-- Copyright (C) 2014 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Text (strip)
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (status200, status405)
import Network.Wai (Application, Request, Response, requestMethod, getRequestBodyChunk, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Process (CreateProcess(..), StdStream(..), proc, createProcess, waitForProcess)
import System.IO (hPutStr, hClose, hSetBinaryMode)
import System.IO.Temp (withSystemTempFile)

port :: Int
port = 8080

main :: IO ()
main = do
  putStrLn $ "tersmu REST API listening on http://0.0.0.0:" ++ show port
  run port app

app :: Application
app request respond = do
  let method = requestMethod request
  let path = pathInfo request
  case (method, path) of
    ("GET", []) -> respond $ ok "tersmu parser API. POST text to / or /parse to get logical form.\n"
    ("GET", ["health"]) -> respond $ ok "ok\n"
    ("POST", []) -> handleParse request respond
    ("POST", ["parse"]) -> handleParse request respond
    _ -> respond $ responseLBS status405 [] "Method Not Allowed\n"

ok :: String -> Response
ok body = responseLBS status200 [("Content-Type", "text/plain; charset=utf-8")] (encodeUtf8 (pack body))

handleParse :: Request -> (Response -> IO a) -> IO a
handleParse request respond = do
  body <- strictRequestBody request
  let input = unpack $ decodeUtf8 $ BL.fromStrict body
  -- Drop trailing empty lines so POST body matching .jbo files (with trailing newline) matches expected .loj
  let lines' = reverse . dropWhile null . reverse $ lines input
  outBytes <- withSystemTempFile "tersmu_in.jbo" $ \path h -> do
    mapM_ (\line -> hPutStr h (line ++ "\n")) lines'
    hClose h
    -- IMPORTANT: read stdout as bytes to avoid locale-based truncation/corruption
    -- (e.g. when output contains non-ASCII like '∧').
    (_hin, Just hout, _herr, ph) <-
      createProcess
        (proc "tersmu" ["--json", "-L", path])
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
    hSetBinaryMode hout True
    bs <- BL.hGetContents hout
    _ <- waitForProcess ph
    pure bs

  -- tersmu --json is documented as NDJSON (one JSON object per line). We split on '\n'
  -- at the byte level to avoid locale decoding issues.
  let jsonLines = filter (not . BL.null) (BL.split 10 outBytes)
  let decodeUtf8Lenient =
        TE.decodeUtf8With TEE.lenientDecode . BL.toStrict
  let parseJsonLine bs =
        maybe
          (object ["error" .= ("decode failed" :: String), "raw" .= decodeUtf8Lenient bs])
          trimJsonValue
          (decode bs :: Maybe Value)
  let decoded = map parseJsonLine jsonLines
  let responseBody = case decoded of
        [one] -> one
        many -> object ["results" .= many]
  respond $ responseLBS status200 [("Content-Type", "application/json; charset=utf-8")] (encode responseBody)

-- Trim spaces and newlines from string values in JSON; trim all string leaves recursively
trimJsonValue :: Value -> Value
trimJsonValue (String t) = String (strip t)
trimJsonValue (Object o) = Object (KM.map trimJsonValue o)
trimJsonValue (Array a) = Array (fmap trimJsonValue a)
trimJsonValue x = x

strictRequestBody :: Request -> IO B.ByteString
strictRequestBody req = go B.empty
  where
    go acc = do
      chunk <- getRequestBodyChunk req
      if B.null chunk then return acc else go (acc <> chunk)

