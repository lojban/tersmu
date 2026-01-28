-- This file is part of tersmu
-- Copyright (C) 2014 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Main where

import ParseText (parseText)
import JboParse (evalText, evalStatement)
import JboSyntax
import ParseM (ParseStateT, evalParseStateT, evalParseStateM)
import JboShow
import Logic
import Bindful
import Morph

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Either
import System.IO
import System.IO.Error
import System.Exit
import System.Process
import System.Environment
import System.Console.GetOpt

versionString = "0.2.2"

doParse :: [Opt] -> Handle -> Handle -> String -> IO ()
doParse opts h herr s = case morph s of
    Left errpos -> highlightError herr errpos s "Morphology error"
    Right text -> evalParseStateT $ showParsedText opts h herr text $ parseText text

showParsedText :: [Opt] -> Handle -> Handle -> String -> Either Int Text -> ParseStateT IO ()
showParsedText opts h _ s (Right text) = do
    let outType = last $ Both:[t | Output t <- opts]
    jboText <- mapStateT (return.runIdentity) $ JboParse.evalText text
    when (not $ null jboText) $ do
        liftIO $ hPutStr h $ (if Utf8 `elem` opts then id else asciifyJboShown) $ concat
	    [if not $ (jbo && outType == Loj) || (not jbo && outType == Jbo)
		then evalBindful (logjboshow jbo jboText) ++ "\n\n"
		else ""
	    | jbo <- [False,True]
	    ]
showParsedText _ _ herr s (Left pos) = highlightError herr pos s "Parse error"

highlightError h pos s errstr = let context = 40 in
    liftIO $ hPutStr h $ errstr++":" ++
	"\n\t{" ++ take (context*2) (drop (pos-context) s) ++ "}" ++
	"\n\t " ++ replicate (min pos context) ' ' ++
	"^" ++
	"\n\n"

-- Trim spaces and newlines from both ends
trimStr :: String -> String
trimStr = reverse . dropWhile (`elem` " \t\n\r") . reverse . dropWhile (`elem` " \t\n\r")

errorMessage :: String -> Int -> String -> String
errorMessage errstr pos s = let context = 40 in
    errstr++":" ++
	"\n\t{" ++ take (context*2) (drop (pos-context) s) ++ "}" ++
	"\n\t " ++ replicate (min pos context) ' ' ++
	"^" ++
	"\n\n"

-- Parse one line to either error message or (logical_form, canonical_form)
parseLineToResult :: String -> Either String (String, String)
parseLineToResult s = case morph s of
    Left errpos -> Left $ errorMessage "Morphology error" errpos s
    Right textStr -> case parseText textStr of
	Left pos -> Left $ errorMessage "Parse error" pos textStr
	Right text ->
	    let jboText = evalParseStateM (JboParse.evalText text)
		logical = evalBindful (logjboshow False jboText)
		canonical = evalBindful (logjboshow True jboText)
	    in Right (logical, canonical)

jsonEscape :: String -> String
jsonEscape = concatMap $ \c -> case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> [c]

jsonOneLine :: String -> Either String (String, String) -> String
jsonOneLine input result = case result of
    Left err -> "{\"input\":\"" ++ jsonEscape (trimStr input) ++ "\",\"logical\":null,\"canonical\":null,\"error\":\"" ++ jsonEscape (trimStr err) ++ "\"}"
    Right (loj, jbo) -> "{\"input\":\"" ++ jsonEscape (trimStr input) ++ "\",\"logical\":\"" ++ jsonEscape (trimStr loj) ++ "\",\"canonical\":\"" ++ jsonEscape (trimStr jbo) ++ "\",\"error\":null}"

data OutputType = Jbo | Loj | Both
    deriving (Eq, Ord, Show)
data InputType = WholeText | Paras | Lines
    deriving (Eq, Ord, Show)
data Opt = Output OutputType | Input InputType | Utf8 | Help | Version | Json
    deriving (Eq, Ord, Show)
options =
    [ Option ['l'] ["loj"] (NoArg (Output Loj)) "output logical form only"
    , Option ['j'] ["jbo"] (NoArg (Output Jbo)) "output forethoughtful lojbanic form only"
    , Option ['L'] ["lines"] (NoArg (Input Lines)) "interpret each line as a lojban text"
    , Option ['p'] ["paragraphs"] (NoArg (Input Paras)) "interpret each blank-line-separated paragraph as a lojban text"
    , Option ['u'] ["utf8"] (NoArg Utf8) "output utf8 encoded text rather than ascii"
    , Option ['v'] ["version"] (NoArg Version) "show version"
    , Option ['h'] ["help"] (NoArg Help) "show help"
    , Option [] ["json"] (NoArg Json) "output one JSON object per line (NDJSON) with input, logical, canonical, error"
    ]
parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
	(o,_,[]) | Help `elem` o -> putStrLn (usageInfo header options) >> exitWith ExitSuccess
	(o,_,[]) | Version `elem` o -> putStrLn versionString >> exitWith ExitSuccess
	(o,n,[]) -> return (o,n)
	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: tersmu [OPTION...] [in] [out]\n\t(use '-' for stdin/stdout)"
main :: IO ()
main = do
    (opts,args) <- getArgs >>= parseArgs
    let inType = last $ WholeText:[t | Input t <- opts]
    (inf, h) <- case args of
	[] -> return (Nothing,stdout)
	[infn] -> do
	    s <- if infn == "-" then getContents else readFile infn
	    return (Just s,stdout)
	[infn,outfn] -> do
	    s <- if infn == "-" then getContents else readFile infn
	    h <- if outfn == "-" then return stdout else openFile outfn WriteMode
	    return (Just s, h)
    case inf of
	Nothing -> repl opts h `catchIOError` (\e ->
	    if isEOFError e then exitWith ExitSuccess
		else putStr (show e) >> exitFailure)
	Just s -> if Json `elem` opts
	    then mapM_ (hPutStrLn h . (\line -> jsonOneLine line (parseLineToResult line))) (mangleInput inType s) >> hClose h
	    else mapM (doParse opts h stderr) (mangleInput inType s) >> hClose h
    where
	repl opts h = do
	    -- interactive mode
	    hPutStr stderr "> "
	    hFlush stderr
	    s <- getLine
	    hPutStrLn stderr ""
	    if Json `elem` opts
		then hPutStrLn h $ jsonOneLine s (parseLineToResult s)
		else doParse opts h stderr s
	    repl opts h
	mangleInput WholeText = (\x -> [x]) . map (\c -> if c `elem` "\n\r" then ' ' else c)
	mangleInput Lines = lines
	mangleInput Paras = map (intercalate " ") . splitAtNulls . lines
	splitAtNulls ls = let (h,t) = break null ls in
	    h : if null t then [] else splitAtNulls (tail t)
