module Main where

import MemoAnalysis
import Options
import Pappy
import RawFiles
import ReadGrammar
import ReduceGrammar
import SimplifyGrammar
import WriteParser

parseImports gram _ [] = return gram
parseImports gram ds (x:xs) | x `elem` ds = parseImports gram ds xs
parseImports gram ds (x:xs) = do
    ngram <- pappyParseFile x
    let pgram = gram { grammarNonterminals = grammarNonterminals gram ++ grammarNonterminals ngram }
    parseImports pgram (x:ds) (xs ++ grammarImports ngram)

processFile flags inname outname = do
    gram@Grammar { grammarImports = is }  <- pappyParseFile inname
    gram@Grammar { grammarNonterminals = g } <- parseImports gram [inname] is
    putStrLn ("Original grammar: " ++ show (length g) ++ " size " ++ show (sizeofNonterminals g))
    --mapM_ print $ grammarNonterminals gram
    --putStrLn (showNonterminals g)
    let gram'@Grammar { grammarNonterminals = g' } = reduceGrammar gram
    putStrLn ("Reduced grammar: " ++ show (length g') ++ " size " ++ show (sizeofNonterminals g'))
    --putStrLn (showNonterminals g')
    let gram''@Grammar { grammarNonterminals = g'' }  = simplifyGrammar gram'
    putStrLn ("Simplified grammar: " ++ show (length g'') ++ " size " ++ show (sizeofNonterminals g''))
    --putStrLn (showNonterminals g'')

    gram'' <- return gram'' { grammarOptions = flags }

    let m'' = memoAnalysis g''
    putStrLn ("Memoized: " ++ show (length m''))

    parser <- writeParser m'' gram''
    sa <- standaloneCode
    writeFile outname (parser ++ sa)

standaloneCode = do
    let f xs = unlines . dropWhile (/= "-- BEGIN CODE") $ lines xs
    sa <- isStandalone
    md <- isMonad
    pappypos_hs <- procFile pappypos_hs
    pappyparse_hs <- procFile pappyparse_hs
    pappybasic_hs <- procFile pappybasic_hs
    case (sa,md) of
        (False,_) -> return ""
        (True,True) -> return $ f pappypos_hs ++ f pappybasic_hs ++ f pappyparse_hs
        (True,False) -> return $ f pappypos_hs ++ f pappybasic_hs

main = do
    (fs ,inname, outname) <- processArgs
    putVerboseLn $ "Input " ++ inname ++ "\nOutput: " ++ outname
    processFile fs inname outname
