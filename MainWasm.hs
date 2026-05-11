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
import JboProp
import Logic
import Bindful
import Morph

import JboTree (jboPropToGraph, jboPropsToGraph, jboTextToGraph, toJson)
import Data.List (intercalate, isPrefixOf)

import Data.Char
import Data.Either
import Foreign.C.String
import Foreign.Marshal.Alloc (free)

-- Simple parsing function that takes a string and returns JSON-like result
-- Returns: Right (logical, canonical, graph_json) or Left error_message
parseLineToResult :: String -> Either String (String, String, String)
parseLineToResult = parseLineToResultWithGraph True

parseLineToResultWithGraph :: Bool -> String -> Either String (String, String, String)
parseLineToResultWithGraph includeGraph s = parseLineToResultWithGraphAndText id includeGraph s

parseLineToResultWithGraphAndText :: (JboText -> JboText) -> Bool -> String -> Either String (String, String, String)
parseLineToResultWithGraphAndText adjustText includeGraph s = case morph s of
    Left errpos -> Left $ errorMessage "Morphology error" errpos s
    Right textStr -> case parseText textStr of
        Left pos -> Left $ errorMessage "Parse error" pos textStr
        Right text ->
            let jboText = adjustText $ evalParseStateM (JboParse.evalText text)
                logical = evalBindful (logjboshow False jboText)
                canonical = evalBindful (logjboshow True jboText)
                graphJson = if includeGraph then toJson . jboTextToGraph $ jboText else "null"
            in Right (logical, canonical, graphJson)

errorMessage :: String -> Int -> String -> String
errorMessage errstr pos s = let context = 40 in
    errstr++":" ++
    "\n\t{" ++ take (context*2) (drop (pos-context) s) ++ "}" ++
    "\n\t " ++ replicate (min pos context) ' ' ++
    "^"

-- Trim spaces and newlines from both ends
trimStr :: String -> String
trimStr = reverse . dropWhile (`elem` " \t\n\r") . reverse . dropWhile (`elem` " \t\n\r")

trimLines :: String -> String
trimLines = reverse . dropWhile (`elem` "\n\r") . reverse . dropWhile (`elem` "\n\r")

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
resultToJson :: (String -> String) -> String -> Either String (String, String, String) -> String
resultToJson enc input result = resultToJsonWithError enc id input result

resultToJsonWithError :: (String -> String) -> (String -> String) -> String -> Either String (String, String, String) -> String
resultToJsonWithError enc errEnc input result =
    case result of
        Left err ->
            "{\"input\":\"" ++ jsonEscape (trimStr input) ++
            "\",\"logical\":null,\"canonical\":null,\"graph\":null,\"error\":\"" ++
            jsonEscape (errEnc (trimStr (enc err))) ++ "\"}"
        Right (loj, jbo, graph) ->
            "{\"input\":\"" ++ jsonEscape (trimStr input) ++
            "\",\"logical\":\"" ++ jsonEscape (trimLines (enc loj)) ++
            "\",\"canonical\":\"" ++ jsonEscape (trimLines (enc jbo)) ++
            "\",\"graph\":" ++ graph ++
            ",\"error\":null}"

-- Main exported function: parse a Lojban string and return UTF-8 JSON
parseLojban :: String -> String
parseLojban input = resultToJson id input (parseLineToResult input)

legacyAsciifyJboShown :: String -> String
legacyAsciifyJboShown = legacyAsciiSymbols . asciifyJboShown
  where
    legacyAsciiSymbols [] = []
    legacyAsciiSymbols ('<':'=':cs) = "=<" ++ legacyAsciiSymbols cs
    legacyAsciiSymbols ('<':'-':'>':cs) = "<->" ++ legacyAsciiSymbols cs
    legacyAsciiSymbols ('-':'>':cs) = "-->" ++ legacyAsciiSymbols cs
    legacyAsciiSymbols (c:cs) = c : legacyAsciiSymbols cs

legacyTexticule :: Texticule -> Texticule
legacyTexticule (TexticuleSide _ t) = legacyTexticule t
legacyTexticule (TexticuleProp p) = TexticuleProp $ legacyProp p
legacyTexticule (TexticuleFrag f) = TexticuleFrag $ legacyFragment f

legacyFragment :: JboFragment -> JboFragment
legacyFragment (JboFragTerms ts) = JboFragTerms $ map legacyTerm ts
legacyFragment f = f

legacyProp :: JboProp -> JboProp
legacyProp (Not p) = Not $ legacyProp p
legacyProp (Connected c p1 p2) = Connected c (legacyProp p1) (legacyProp p2)
legacyProp (NonLogConnected c p1 p2) = NonLogConnected c (legacyProp p1) (legacyProp p2)
legacyProp (Quantified q r p) = Quantified q (fmap (\r' v -> legacyProp $ r' v) r) (\v -> legacyProp $ p v)
legacyProp (Modal o p) = Modal (legacyModalOp o) (legacyProp p)
legacyProp (Rel r ts) = Rel (legacyRel r) $ map legacyTerm ts
legacyProp Eet = Eet

legacyModalOp :: JboModalOp -> JboModalOp
legacyModalOp (JboTagged tag mt) = JboTagged tag $ legacyTerm <$> mt
legacyModalOp (WithEventAs t) = WithEventAs $ legacyTerm t
legacyModalOp o = o

legacyRel :: JboRel -> JboRel
legacyRel (ScalarNegatedRel n r) = ScalarNegatedRel n $ legacyRel r
legacyRel (AbsProp a p) = AbsProp a $ legacyProp p
legacyRel (Moi t m) = Moi (legacyTerm t) m
legacyRel (Among t) = Among $ legacyTerm t
legacyRel r = r

legacyTerm :: JboTerm -> JboTerm
legacyTerm (TermWithSides t _) = legacyTerm t
legacyTerm (Constant n ts) = Constant n $ map legacyTerm ts
legacyTerm (JboQuote (ParsedQuote ts)) = JboQuote (ParsedQuote $ map legacyTexticule ts)
legacyTerm (JoikedTerms joik t1 t2) = JoikedTerms joik (legacyTerm t1) (legacyTerm t2)
legacyTerm (QualifiedTerm qual t) = QualifiedTerm qual $ legacyTerm t
legacyTerm t = t

legacyErrorMessage :: String -> String
legacyErrorMessage err
    | "Morphology error:" `isPrefixOf` err = case break (== '}') err of
        (prefix, '}':rest) -> prefix ++ " }" ++ rest
        _ -> err
    | otherwise = err

-- Compatibility export for old ASCII goldens
parseLojbanAscii :: String -> String
parseLojbanAscii input = resultToJsonWithError legacyAsciifyJboShown legacyErrorMessage input (parseLineToResultWithGraphAndText (map legacyTexticule) False input)

-- FFI wrapper for parseLojban
parseLojbanWasm :: CString -> IO CString
parseLojbanWasm inputPtr = do
    input <- peekCString inputPtr
    let result = parseLojban input
    newCString result

parseLojbanAsciiWasm :: CString -> IO CString
parseLojbanAsciiWasm inputPtr = do
    input <- peekCString inputPtr
    let result = parseLojbanAscii input
    newCString result

-- Exported initializer
initTersmu :: IO ()
initTersmu = return () -- GHC RTS handles its own init if called via exported functions, but sometimes we need a dummy or explicit call

-- Export for WASM (using C API FFI)
foreign export ccall "parseLojban" parseLojbanWasm :: CString -> IO CString
foreign export ccall "parseLojbanAscii" parseLojbanAsciiWasm :: CString -> IO CString
foreign export ccall "initTersmu" initTersmu :: IO ()

main :: IO ()
main = return ()
