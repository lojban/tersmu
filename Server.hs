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
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (status200, status405)
import Network.Wai (Application, Request, Response, requestMethod, requestBody, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Process (readProcessWithExitCode)
import System.IO (hPutStr, hClose)
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
  chunks <- mapM parseOneLine lines'
  let out = concat chunks
  respond $ responseLBS status200 [("Content-Type", "text/plain; charset=utf-8")] (encodeUtf8 (pack out))

strictRequestBody :: Request -> IO B.ByteString
strictRequestBody req = go B.empty
  where
    go acc = do
      chunk <- requestBody req
      if B.null chunk then return acc else go (acc <> chunk)

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Use a temp file so tersmu reads from file (no "> " prompt) rather than stdin/repl
parseOneLine :: String -> IO String
parseOneLine line = do
  let trimmed = trim line
  (_, out, err) <- withSystemTempFile "tersmu_in.jbo" $ \path h -> do
    hPutStr h (line ++ "\n")
    hClose h
    readProcessWithExitCode "tersmu" ["-L", path] ""
  -- Trim trailing newlines from parser output; expected .loj has one blank line before -----
  let block = reverse . dropWhile (== '\n') . reverse $ out ++ err
  -- Empty lines: "> \n\n-----\n"; non-empty: "> line\n\n<block>\n\n-----\n"
  if null block
    then return $ "> " ++ trimmed ++ "\n\n-----\n"
    else return $ "> " ++ trimmed ++ "\n\n" ++ block ++ "\n\n-----\n"
