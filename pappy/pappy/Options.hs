module Options where

import System.Console.GetOpt
import System.IO
import System.Environment
import System.Exit

import RawFiles

data Flag = Verbose | Output String  | External | WriteFiles | InstDerivs | Lang String
    deriving(Eq,Show)

putErrLn = hPutStrLn stderr

options :: [OptDescr Flag]
options =
     [ Option ['v']  ["verbose"]     (NoArg Verbose)         "chatty output on stderr"
     , Option ['o']  ["output"]      (ReqArg Output "FILE")  "output FILE"
     , Option []     ["monad"]       (NoArg InstDerivs)      "declare a Derivs instance for your parser"
     , Option ['e']  []              (NoArg External)        "use external pappy files"
     , Option []     ["98"]          (NoArg (Lang "98"))     "Create Haskell 98 code"
     , Option []     ["2010"]        (NoArg (Lang "2010"))   "Create Haskell 2010 code"
     , Option []     ["write-files"] (NoArg WriteFiles)      "create pappy files in the current directory"
     ]

putVerboseLn s  = do
    (fs,_,_) <- processArgs
    if Verbose `notElem` fs then return () else putErrLn s

isStandalone = do
    (fs,_,_) <- processArgs
    return (External `notElem` fs)

isMonad = do
    (fs,_,_) <- processArgs
    return (InstDerivs `elem` fs)

procFile s = do
    (fs,_,_) <- processArgs
    return (fixImports fs s)

outputLang = do
    (fs,_,_) <- processArgs
    return $ last ("98":[ s | Lang s <- fs])

fixImports o s | Lang "2010" `elem` o = unlines (map f (lines s)) where
    f ('i':'m':'p':'o':'r':'t':' ':'P':'a':'p':'p':'y':rs) = "import Pappy." ++ rs
    f "import Char" = "import Data.Char"
    f "import List" = "import Data.List"
    f x = g x
    g ('m':'o':'d':'u':'l':'e':' ':'P':'a':'p':'p':'y':rs) = "module Pappy." ++ g rs
    g (x:xs) = x:g xs
    g [] = []
fixImports _ s = s

processArgs = do
    argv <- getArgs
    let printUsage err =  putErrLn (err ++ usageInfo header options) >> exitFailure
        header = "Usage: pappy [OPTIONS ..] Input.pappy"
        mkoutname inname = case reverse inname of
            'y':'p':'p':'a':'p':'.':inr -> reverse inr ++ ".hs"
            _ -> inname ++ ".hs"
    case getOpt Permute options argv of
        (o,_,[]) | WriteFiles `elem` o && Lang "2010" `elem` o -> do
            putStrLn "creating Pappy/*.hs"
            writeFile "Pappy/Parse.hs" (fixImports o pappyparse_hs)
            writeFile "Pappy/Pos.hs" (fixImports o pappypos_hs)
            writeFile "Pappy/Basic.hs" (fixImports o pappybasic_hs)
            exitWith ExitSuccess
        (o,_,[]) | WriteFiles `elem` o -> do
            writeFile "PappyParse.hs" pappyparse_hs
            writeFile "PappyPos.hs" pappypos_hs
            writeFile "PappyBasic.hs" pappybasic_hs
            exitWith ExitSuccess
        (o,[infile],[]) -> return (o,infile,head $ [ fs | Output fs <- o ] ++ [mkoutname infile])
        (o,[],[]) -> printUsage "No pappy file specified\n"
        (_,_,errs) -> printUsage (concat errs)
