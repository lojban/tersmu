-- WASM entry point for tersmu parser
-- This module exports a simple function that can be called from JavaScript
--
-- This file is part of tersmu
-- Copyright (C) 2014 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Main where

import ParseText (parseText)
import JboParse (evalText)
import JboSyntax
import ParseM (evalParseStateM)
import JboShow
import Logic
import Bindful
import Morph

import JboTree (jboPropToTree, toJson)
import JboProp (propTexticules)
import Data.List (intercalate)

import Data.Char
import Data.Either
import Foreign.C.String
import Foreign.Marshal.Alloc (free)

-- Simple parsing function that takes a string and returns JSON-like result
-- Returns: Right (logical, canonical, tree_json) or Left error_message
parseLineToResult :: String -> Either String (String, String, String)
parseLineToResult s = case morph s of
    Left errpos -> Left $ errorMessage "Morphology error" errpos s
    Right textStr -> case parseText textStr of
        Left pos -> Left $ errorMessage "Parse error" pos textStr
        Right text ->
            let jboText = evalParseStateM (JboParse.evalText text)
                logical = evalBindful (logjboshow False jboText)
                canonical = evalBindful (logjboshow True jboText)
                treeJson = "[" ++ intercalate "," (map (toJson . jboPropToTree) (propTexticules jboText)) ++ "]"
            in Right (logical, canonical, treeJson)

errorMessage :: String -> Int -> String -> String
errorMessage errstr pos s = let context = 40 in
    errstr++":" ++
    "\n\t{" ++ take (context*2) (drop (pos-context) s) ++ "}" ++
    "\n\t " ++ replicate (min pos context) ' ' ++
    "^"

-- Trim spaces and newlines from both ends
trimStr :: String -> String
trimStr = reverse . dropWhile (`elem` " \t\n\r") . reverse . dropWhile (`elem` " \t\n\r")

-- JSON escape function
jsonEscape :: String -> String
jsonEscape = concatMap $ \c -> case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> [c]

-- Convert result to JSON string
resultToJson :: String -> Either String (String, String, String) -> String
resultToJson input result =
    case result of
        Left err ->
            "{\"input\":\"" ++ jsonEscape (trimStr input) ++
            "\",\"logical\":null,\"canonical\":null,\"tree\":null,\"error\":\"" ++
            jsonEscape (trimStr err) ++ "\"}"
        Right (loj, jbo, tree) ->
            "{\"input\":\"" ++ jsonEscape (trimStr input) ++
            "\",\"logical\":\"" ++ jsonEscape (trimStr loj) ++
            "\",\"canonical\":\"" ++ jsonEscape (trimStr jbo) ++
            "\",\"tree\":" ++ tree ++
            ",\"error\":null}"

-- Main exported function: parse a Lojban string and return JSON
parseLojban :: String -> String
parseLojban input = resultToJson input (parseLineToResult input)

-- FFI wrapper for parseLojban
parseLojbanWasm :: CString -> IO CString
parseLojbanWasm inputPtr = do
    input <- peekCString inputPtr
    let result = parseLojban input
    newCString result

-- Exported initializer
initTersmu :: IO ()
initTersmu = return () -- GHC RTS handles its own init if called via exported functions, but sometimes we need a dummy or explicit call

-- Export for WASM (using C API FFI)
foreign export ccall "parseLojban" parseLojbanWasm :: CString -> IO CString
foreign export ccall "initTersmu" initTersmu :: IO ()

main :: IO ()
main = return ()
