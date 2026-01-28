module RawFiles where


-- | Generated from PappyPos.hs on Wed Jan 28 20:12:17 IST 2026
{-# NOINLINE pappypos_hs #-}
pappypos_hs :: String
pappypos_hs = "\
 \-- Simple data type to keep track of character positions\n\
 \-- within a text file or other text stream.\n\
 \module PappyPos where\n\
 \\n\
 \-- BEGIN CODE\n\
 \\n\
 \-- Basic position indicator data type: filename, line number, column number.\n\
 \data Pos = Pos {\n\
 \\x0009\&\x0009\&posFile\x0009\&:: !String,\n\
 \\x0009\&\x0009\&posLine\x0009\&:: !Int,\n\
 \\x0009\&\x0009\&posCol\x0009\&:: !Int\n\
 \\x0009\&}\n\
 \\n\
 \-- Incrementally compute the next position in a text file\n\
 \-- if 'c' is the character at the current position.\n\
 \-- Follows the standard convention of 8-character tab stops.\n\
 \nextPos (Pos file line col) c =\n\
 \\x0009\&if c == '\\n' then Pos file (line + 1) 1\n\
 \\x0009\&else if c == '\\t' then Pos file line ((div (col + 8 - 1) 8) * 8 + 1)\n\
 \\x0009\&else Pos file line (col + 1)\n\
 \\n\
 \-- Two positions are equal if each of their elements are equal.\n\
 \instance Eq Pos where\n\
 \\x0009\&Pos f1 l1 c1 == Pos f2 l2 c2 =\n\
 \\x0009\&\x0009\&f1 == f2 && l1 == l2 && c1 == c2\n\
 \\n\
 \-- Two positions are ordered by line number, then column number.\n\
 \instance Ord Pos where\n\
 \\x0009\&Pos f1 l1 c1 <= Pos f2 l2 c2 =\n\
 \\x0009\&\x0009\&(l1 < l2) || (l1 == l2 && c1 <= c2)\n\
 \\n\
 \-- Standard way to display positions - \"file:line:col\"\n\
 \instance Show Pos where\n\
 \\x0009\&show (Pos file line col) = file ++ \":\" ++ show line ++ \":\" ++ show col\n\
 \\n\
 \-- Show a position relative to a base position.\n\
 \-- If the new position is in the same line, just show its column number;\n\
 \-- otherwise if the new position is in the same file,\n\
 \-- just show its line and column numbers;\n\
 \-- otherwise show the complete new position.\n\
 \showPosRel (Pos file line col) (Pos file' line' col') =\n\
 \\x0009\&if (file == file')\n\
 \\x0009\&then\x0009\&if (line == line')\n\
 \\x0009\&\x0009\&then \"column \" ++ show col'\n\
 \\x0009\&\x0009\&else \"line \" ++ show line' ++ \", column \" ++ show col'\n\
 \\x0009\&else show (Pos file' line' col')\n\
 \"

-- | Generated from PappyParse.hs on Wed Jan 28 20:12:17 IST 2026
{-# NOINLINE pappyparse_hs #-}
pappyparse_hs :: String
pappyparse_hs = "\
 \{-# LANGUAGE CPP #-}\n\
 \\n\
 \module PappyParse(module PappyParse, module PappyBasic, module PappyPos) where\n\
 \\n\
 \-- This contains the parsing monad and is only needed with the --monad option.\n\
 \-- It allows mixing handwritten and pappy generated parsers.\n\
 \\n\
 \import Data.Char\n\
 \import Control.Applicative (Applicative(..))\n\
 \import Control.Monad\n\
 \\n\
 \-- Control.Monad.Fail import redundant in GHC 8.8+\n\
 \import qualified Control.Monad.Fail as Fail\n\
 \\n\
 \import PappyPos\n\
 \import PappyBasic\n\
 \\n\
 \-- BEGIN CODE\n\
 \\n\
 \newtype Parser d v = Parser { unParser :: d -> Result d v }\n\
 \\n\
 \class Derivs d where\n\
 \\x0009\&dvPos\x0009\&:: d -> Pos\n\
 \\x0009\&dvChar\x0009\&:: d -> Result d Char\n\
 \\n\
 \---------- Basic parsing combinators\n\
 \\n\
 \infixl 2 </>\x0009\&\x0009\&-- ordered choice\n\
 \infixl 1 <?>\x0009\&\x0009\&-- error labeling\n\
 \infixl 1 <?!>\x0009\&\x0009\&-- unconditional error labeling\n\
 \\n\
 \-- Standard monadic combinators\n\
 \instance Derivs d => Monad (Parser d) where\n\
 \\n\
 \\x0009\&-- Sequencing combinator\n\
 \\x0009\&(Parser p1) >>= f = Parser parse\n\
 \\n\
 \\x0009\&\x0009\&where parse dvs = first (p1 dvs)\n\
 \\n\
 \\x0009\&\x0009\&      first (Parsed val rem err) =\n\
 \\x0009\&\x0009\&\x0009\&let Parser p2 = f val\n\
 \\x0009\&\x0009\&\x0009\&in second err (p2 rem)\n\
 \\x0009\&\x0009\&      first (NoParse err) = NoParse err\n\
 \\n\
 \\x0009\&\x0009\&      second err1 (Parsed val rem err) =\n\
 \\x0009\&\x0009\&\x0009\&Parsed val rem (joinErrors err1 err)\n\
 \\x0009\&\x0009\&      second err1 (NoParse err) =\n\
 \\x0009\&\x0009\&\x0009\&NoParse (joinErrors err1 err)\n\
 \\n\
 \\x0009\&-- Result-producing combinator\n\
 \\x0009\&return x = Parser (\\dvs -> Parsed x dvs (nullError dvs))\n\
 \\n\
 \#if !MIN_VERSION_base(4,13,0)\n\
 \        -- Monad(fail) removed in GHC 8.8+\n\
 \        fail = Fail.fail\n\
 \#endif\n\
 \\n\
 \instance Derivs d => Fail.MonadFail (Parser d) where\n\
 \\x0009\&-- Failure combinator\n\
 \\x0009\&fail [] = Parser (\\dvs -> NoParse (nullError dvs))\n\
 \\x0009\&fail msg = Parser (\\dvs -> NoParse (msgError (dvPos dvs) msg))\n\
 \\n\
 \instance Derivs d => Functor (Parser d) where\n\
 \    fmap f action = do r <- action; return (f r)\n\
 \\n\
 \instance Derivs d => Applicative (Parser d) where\n\
 \    pure = return\n\
 \    (<*>) = ap\n\
 \\n\
 \-- Ordered choice\n\
 \(</>) :: Derivs d => Parser d v -> Parser d v -> Parser d v\n\
 \(Parser p1) </> (Parser p2) = Parser parse\n\
 \\n\
 \\x0009\&\x0009\&where parse dvs = first dvs (p1 dvs)\n\
 \\n\
 \\x0009\&\x0009\&      first dvs (result@(Parsed val rem err)) = result\n\
 \\x0009\&\x0009\&      first dvs (NoParse err) = second err (p2 dvs)\n\
 \\n\
 \\x0009\&\x0009\&      second err1 (Parsed val rem err) =\n\
 \\x0009\&\x0009\&\x0009\&Parsed val rem (joinErrors err1 err)\n\
 \\x0009\&\x0009\&      second err1 (NoParse err) =\n\
 \\x0009\&\x0009\&\x0009\&NoParse (joinErrors err1 err)\n\
 \\n\
 \-- Semantic predicate: 'satisfy <parser> <pred>' acts like <parser>\n\
 \-- but only succeeds if the result it generates satisfies <pred>.\n\
 \satisfy :: Derivs d => Parser d v -> (v -> Bool) -> Parser d v\n\
 \satisfy (Parser p) test = Parser parse\n\
 \\n\
 \\x0009\&\x0009\&where parse dvs = check dvs (p dvs)\n\
 \\n\
 \\x0009\&\x0009\&      check dvs (result@(Parsed val rem err)) =\n\
 \\x0009\&\x0009\&\x0009\&if test val then result\n\
 \\x0009\&\x0009\&\x0009\&\x0009\&    else NoParse (nullError dvs)\n\
 \\x0009\&\x0009\&      check dvs none = none\n\
 \\n\
 \-- Syntactic predicate: 'followedBy <parser>' acts like <parser>\n\
 \-- but does not consume any input.\n\
 \followedBy :: Derivs d => Parser d v -> Parser d v\n\
 \followedBy (Parser p) = Parser parse\n\
 \\n\
 \\x0009\&where parse dvs = case (p dvs) of\n\
 \\x0009\&\x0009\&Parsed val rem err -> Parsed val dvs (nullError dvs)\n\
 \\x0009\&\x0009\&err -> err\n\
 \\n\
 \-- Negative syntactic predicate: 'followedBy <parser>' invokes <parser>,\n\
 \-- then succeeds without consuming any input if <parser> fails,\n\
 \-- and fails if <parser> succeeds.\n\
 \notFollowedBy :: Derivs d => Parser d v -> Parser d ()\n\
 \notFollowedBy (Parser p) = Parser parse\n\
 \\n\
 \\x0009\&where parse dvs = case (p dvs) of\n\
 \\x0009\&\x0009\&Parsed val rem err -> NoParse (nullError dvs)\n\
 \\x0009\&\x0009\&NoParse err -> Parsed () dvs (nullError dvs)\n\
 \\n\
 \-- Optional combinator: 'optional <parser>' invokes <parser>,\n\
 \-- then produces the result 'Just <v>' if <parser> produced <v>,\n\
 \-- or else produces the success result 'Nothing'\n\
 \-- without consuming any input if <parser> failed.\n\
 \optional :: Derivs d => Parser d v -> Parser d (Maybe v)\n\
 \optional p = (do v <- p; return (Just v)) </> return Nothing\n\
 \\n\
 \---------- Iterative combinators\n\
 \-- Note: use of these combinators can break\n\
 \-- a packrat parser's linear-time guarantee.\n\
 \\n\
 \-- Zero or more repetition combinator:\n\
 \-- 'many <parser>' invokes <parser> repeatedly until it fails,\n\
 \-- collecting all success result values into a list.\n\
 \-- Always succeeds, producing an empty list in the degenerate case.\n\
 \many :: Derivs d => Parser d v -> Parser d [v]\n\
 \many p = (do { v <- p; vs <- many p; return (v : vs) } )\n\
 \\x0009\& </> return []\n\
 \\n\
 \-- One or more repetition combinator:\n\
 \-- 'many1 <parser>' invokes <parser> repeatedly until it fails,\n\
 \-- collecting all success result values into a list.\n\
 \-- Fails if <parser> does not succeed even once.\n\
 \many1 :: Derivs d => Parser d v -> Parser d [v]\n\
 \many1 p = do { v <- p; vs <- many p; return (v : vs) }\n\
 \\n\
 \-- One or more repetitions with a separator:\n\
 \-- 'sepBy1 <parser> <separator>' scans one or more iterations of <parser>,\n\
 \-- with a match of <separator> between each instance.\n\
 \-- Only the results of <parser> are collected into the final result list.\n\
 \sepBy1 :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]\n\
 \sepBy1 p psep = do v <- p\n\
 \\x0009\&\x0009\&   vs <- many (do { psep; p })\n\
 \\x0009\&\x0009\&   return (v : vs)\n\
 \\n\
 \-- Zero or more repetitions with a separator:\n\
 \-- like sepBy1, but succeeds with an empty list if nothing can be parsed.\n\
 \sepBy :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]\n\
 \sepBy p psep = sepBy1 p psep </> return []\n\
 \\n\
 \-- Zero or more repetitions with a terminator\n\
 \endBy :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]\n\
 \endBy p pend = many (do { v <- p; pend; return v })\n\
 \\n\
 \-- One or more repetitions with a terminator\n\
 \endBy1 :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]\n\
 \endBy1 p pend = many1 (do { v <- p; pend; return v })\n\
 \\n\
 \-- One or more repetitions with a separator or terminator:\n\
 \-- 'sepEndBy1 <parser> <septerm>' scans for a sequence of <parser> matches\n\
 \-- in which instances are separated by <septerm>,\n\
 \-- and if a <septerm> is found following the last <parser> match\n\
 \-- then it is consumed as well.\n\
 \sepEndBy1 :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]\n\
 \sepEndBy1 p psep = do v <- sepBy1 p psep; optional psep; return v\n\
 \\n\
 \-- Zero or more repetitions with a separator or terminator.\n\
 \sepEndBy :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]\n\
 \sepEndBy p psep = do v <- sepBy p psep; optional psep; return v\n\
 \\n\
 \-- One or more repetitions separated by left-associative operators.\n\
 \-- 'chainl1 <term> <oper>' matches instances of <term> separated by <oper>,\n\
 \-- but uses the result of <oper> as a left-associative binary combinator:\n\
 \-- e.g., 't1 op t2 op t3' is interpreted as '(t1 op t2) op t3'\n\
 \chainl1 :: Derivs d => Parser d v -> Parser d (v->v->v) -> Parser d v\n\
 \chainl1 p psep =\n\
 \\x0009\&let psuffix z = (do f <- psep\n\
 \\x0009\&\x0009\&\x0009\&    v <- p\n\
 \\x0009\&\x0009\&\x0009\&    psuffix (f z v))\n\
 \\x0009\&\x0009\&\x0009\&</> return z\n\
 \\x0009\&in do v <- p\n\
 \\x0009\&      psuffix v\n\
 \\n\
 \-- Zero or more repetitions separated by left-associative operators.\n\
 \chainl :: Derivs d => Parser d v -> Parser d (v->v->v) -> v -> Parser d v\n\
 \chainl p psep z = chainl1 p psep </> return z\n\
 \\n\
 \-- One or more repetitions separated by left-associative operators:\n\
 \-- e.g., 't1 op t2 op t3' is interpreted as 't1 op (t2 op t3)'\n\
 \chainr1 :: Derivs d => Parser d v -> Parser d (v->v->v) -> Parser d v\n\
 \chainr1 p psep = (do v <- p\n\
 \\x0009\&\x0009\&     f <- psep\n\
 \\x0009\&\x0009\&     w <- chainr1 p psep\n\
 \\x0009\&\x0009\&     return (f v w))\n\
 \\x0009\&\x0009\& </> p\n\
 \\n\
 \-- Zero or more repetitions separated by left-associative operators.\n\
 \chainr :: Derivs d => Parser d v -> Parser d (v->v->v) -> v -> Parser d v\n\
 \chainr p psep z = chainr1 p psep </> return z\n\
 \\n\
 \-- N-ary ordered choice:\n\
 \-- given a list of parsers producing results of the same type,\n\
 \-- try them all in order and use the first successful result.\n\
 \choice :: Derivs d => [Parser d v] -> Parser d v\n\
 \choice [p] = p\n\
 \choice (p:ps) = p </> choice ps\n\
 \\n\
 \---------- Error handling\n\
 \failAt :: Derivs d => Pos -> String -> Parser d v\n\
 \failAt pos msg = Parser (\\dvs -> NoParse (msgError pos msg))\n\
 \\n\
 \-- Annotate a parser with a description of the construct to be parsed.\n\
 \-- The resulting parser yields an \"expected\" error message\n\
 \-- if the construct cannot be parsed\n\
 \-- and if no error information is already available\n\
 \-- indicating a position farther right in the source code\n\
 \-- (which would normally be more localized/detailed information).\n\
 \(<?>) :: Derivs d => Parser d v -> String -> Parser d v\n\
 \(Parser p) <?> desc = Parser $ \\dvs ->\n\
 \    errorAnnotate False desc (dvPos dvs) (p dvs)\n\
 \\n\
 \-- Stronger version of the <?> error annotation operator above,\n\
 \-- which unconditionally overrides any existing error information.\n\
 \(<?!>) :: Derivs d => Parser d v -> String -> Parser d v\n\
 \(Parser p) <?!> desc = Parser $ \\dvs ->\n\
 \    errorAnnotate True desc (dvPos dvs) (p dvs)\n\
 \\n\
 \nullError dvs = ParseError (dvPos dvs) []\n\
 \\n\
 \eofError dvs = msgError (dvPos dvs) \"end of input\"\n\
 \\n\
 \expected :: Derivs d => String -> Parser d v\n\
 \expected desc = Parser (\\dvs -> NoParse (expError (dvPos dvs) desc))\n\
 \\n\
 \unexpected :: Derivs d => String -> Parser d v\n\
 \unexpected str = fail (\"unexpected \" ++ str)\n\
 \\n\
 \---------- Character-oriented parsers\n\
 \\n\
 \-- 'anyChar' matches any single character.\n\
 \anyChar :: Derivs d => Parser d Char\n\
 \anyChar = Parser dvChar\n\
 \\n\
 \-- 'char <c>' matches the specific character <c>.\n\
 \char :: Derivs d => Char -> Parser d Char\n\
 \char ch = satisfy anyChar (\\c -> c == ch) <?> show ch\n\
 \\n\
 \-- 'oneOf <s>' matches any character in string <s>.\n\
 \oneOf :: Derivs d => [Char] -> Parser d Char\n\
 \oneOf chs = satisfy anyChar (\\c -> c `elem` chs)\n\
 \\x0009\&    <?> (\"one of the characters \" ++ show chs)\n\
 \\n\
 \-- 'noneOf <s>' matches any character not in string <s>.\n\
 \noneOf :: Derivs d => [Char] -> Parser d Char\n\
 \noneOf chs = satisfy anyChar (\\c -> not (c `elem` chs))\n\
 \\x0009\&     <?> (\"any character not in \" ++ show chs)\n\
 \\n\
 \-- 'string <s>' matches all the characters in <s> in sequence.\n\
 \string :: Derivs d => String -> Parser d String\n\
 \string str = p str <?> show str\n\
 \\n\
 \\x0009\&where p [] = return str\n\
 \\x0009\&      p (ch:chs) = do { char ch; p chs }\n\
 \\n\
 \-- 'stringFrom <ss>' matches any string in the list of strings <ss>.\n\
 \-- If any strings in <ss> are prefixes of other strings in <ss>,\n\
 \-- then the prefixes must appear later in the list\n\
 \-- in order for the longer strings to be recognized at all.\n\
 \stringFrom :: Derivs d => [String] -> Parser d String\n\
 \stringFrom [str] = string str\n\
 \stringFrom (str : strs) = string str </> stringFrom strs\n\
 \\n\
 \-- Match an uppercase letter.\n\
 \upper :: Derivs d => Parser d Char\n\
 \upper = satisfy anyChar isUpper <?> \"uppercase letter\"\n\
 \\n\
 \-- Match a lowercase letter.\n\
 \lower :: Derivs d => Parser d Char\n\
 \lower = satisfy anyChar isLower <?> \"lowercase letter\"\n\
 \\n\
 \-- Match any letter.\n\
 \letter :: Derivs d => Parser d Char\n\
 \letter = satisfy anyChar isAlpha <?> \"letter\"\n\
 \\n\
 \-- Match any letter or digit.\n\
 \alphaNum :: Derivs d => Parser d Char\n\
 \alphaNum = satisfy anyChar isAlphaNum <?> \"letter or digit\"\n\
 \\n\
 \-- Match any digit.\n\
 \digit :: Derivs d => Parser d Char\n\
 \digit = satisfy anyChar isDigit <?> \"digit\"\n\
 \\n\
 \-- Match any hexadecimal digit.\n\
 \hexDigit :: Derivs d => Parser d Char\n\
 \hexDigit = satisfy anyChar isHexDigit <?> \"hexadecimal digit (0-9, a-f)\"\n\
 \\n\
 \-- Match any octal digit.\n\
 \octDigit :: Derivs d => Parser d Char\n\
 \octDigit = satisfy anyChar isOctDigit <?> \"octal digit (0-7)\"\n\
 \\n\
 \-- Match a newline.\n\
 \newline :: Derivs d => Parser d Char\n\
 \newline = char '\\n'\n\
 \\n\
 \-- Match a tab character.\n\
 \tab :: Derivs d => Parser d Char\n\
 \tab = char '\\t'\n\
 \\n\
 \-- Match any whitespace character (space, tab, newline, etc.).\n\
 \space :: Derivs d => Parser d Char\n\
 \space = satisfy anyChar isSpace <?> \"whitespace character\"\n\
 \\n\
 \-- Match a sequence of zero or more whitespace characters.\n\
 \spaces :: Derivs d => Parser d [Char]\n\
 \spaces = many space\n\
 \\n\
 \-- Match the end of file (i.e., \"the absence of a character\").\n\
 \eof :: Derivs d => Parser d ()\n\
 \eof = notFollowedBy anyChar <?> \"end of input\"\n\
 \\n\
 \---------- Parser state manipulation combinators\n\
 \\n\
 \-- Combinator to get the Derivs object for the current position:\n\
 \-- e.g., 'dvs <- getDerivs' as part of a 'do' sequence.\n\
 \getDerivs :: Derivs d => Parser d d\n\
 \getDerivs = Parser (\\dvs -> Parsed dvs dvs (nullError dvs))\n\
 \\n\
 \-- Combinator to set the Derivs object used for subsequent parsing;\n\
 \-- typically used to change parsing state elements in the Derivs tuple.\n\
 \setDerivs :: Derivs d => d -> Parser d ()\n\
 \setDerivs newdvs = Parser (\\dvs -> Parsed () newdvs (nullError dvs))\n\
 \\n\
 \-- Get the current position in the input text.\n\
 \getPos :: Derivs d => Parser d Pos\n\
 \getPos = Parser (\\dvs -> Parsed (dvPos dvs) dvs (nullError dvs))\n\
 \"

-- | Generated from PappyBasic.hs on Wed Jan 28 20:12:17 IST 2026
{-# NOINLINE pappybasic_hs #-}
pappybasic_hs :: String
pappybasic_hs = "\
 \module PappyBasic where\n\
 \\n\
 \-- This module contains basic definitions needed for a pure pappy generated\n\
 \-- parser\n\
 \\n\
 \import PappyPos\n\
 \\n\
 \-- BEGIN CODE\n\
 \\n\
 \---------- Data types used for parsing\n\
 \\n\
 \data ErrorDescriptor =\n\
 \\x0009\&  Expected String\n\
 \   \x0009\&| Message String\n\
 \        deriving(Eq)\n\
 \\n\
 \data ParseError = ParseError {\n\
 \\x0009\&\x0009\&\x0009\&errorPos\x0009\&:: Pos,\n\
 \\x0009\&\x0009\&\x0009\&errorDescrs\x0009\&:: [ErrorDescriptor]\n\
 \\x0009\&\x0009\&}\n\
 \\n\
 \data Result d v =\n\
 \\x0009\&  Parsed v d ParseError\n\
 \        | NoParse ParseError\n\
 \\n\
 \-- Join two ParseErrors, giving preference to the one farthest right,\n\
 \-- or merging their descriptor sets if they are at the same position.\n\
 \joinErrors :: ParseError -> ParseError -> ParseError\n\
 \joinErrors (e@(ParseError p m)) (e'@(ParseError p' m')) =\n\
 \\x0009\&if p' > p || null m then e'\n\
 \\x0009\&else if p > p' || null m' then e\n\
 \\x0009\&else ParseError p (m `union` m') where\n\
 \                union xs (y:ys) = f (reverse xs) ys where\n\
 \                    f xs (y:ys) = if y `elem` xs then f xs ys else f (y:xs) ys\n\
 \                    f xs [] = reverse xs\n\
 \\n\
 \msgError pos msg = ParseError pos [Message msg]\n\
 \\n\
 \-- Comparison operators for ParseError just compare relative positions.\n\
 \instance Eq ParseError where\n\
 \\x0009\&ParseError p1 m1 == ParseError p2 m2\x0009\&= p1 == p2\n\
 \\x0009\&ParseError p1 m1 /= ParseError p2 m2\x0009\&= p1 /= p2\n\
 \\n\
 \instance Ord ParseError where\n\
 \\x0009\&ParseError p1 m1 < ParseError p2 m2\x0009\&= p1 < p2\n\
 \\x0009\&ParseError p1 m1 > ParseError p2 m2\x0009\&= p1 > p2\n\
 \\x0009\&ParseError p1 m1 <= ParseError p2 m2\x0009\&= p1 <= p2\n\
 \\x0009\&ParseError p1 m1 >= ParseError p2 m2\x0009\&= p1 >= p2\n\
 \\n\
 \\x0009\&-- Special behavior: \"max\" joins two errors\n\
 \\x0009\&max p1 p2 = joinErrors p1 p2\n\
 \\x0009\&min p1 p2 = undefined\n\
 \\n\
 \-- Show function for error messages\n\
 \instance Show ParseError where\n\
 \\x0009\&show (ParseError pos []) =\n\
 \\x0009\&\x0009\&show pos ++ \": parse error\"\n\
 \\x0009\&show (ParseError pos msgs) = expectmsg expects ++ messages msgs\n\
 \\x0009\&   where\n\
 \\x0009\&\x0009\&expects = getExpects msgs\n\
 \\x0009\&\x0009\&getExpects [] = []\n\
 \\x0009\&\x0009\&getExpects (Expected exp : rest) = exp : getExpects rest\n\
 \\x0009\&\x0009\&getExpects (Message msg : rest) = getExpects rest\n\
 \\n\
 \\x0009\&\x0009\&expectmsg [] = \"\"\n\
 \\x0009\&\x0009\&expectmsg [exp] = show pos ++ \": expecting \" ++ exp ++ \"\\n\"\n\
 \\x0009\&\x0009\&expectmsg [e1, e2] = show pos ++ \": expecting either \"\n\
 \\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&++ e1 ++ \" or \" ++ e2 ++ \"\\n\"\n\
 \\x0009\&\x0009\&expectmsg (first : rest) = show pos ++ \": expecting one of: \"\n\
 \\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&++ first ++ expectlist rest\n\
 \\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&++ \"\\n\"\n\
 \\x0009\&\x0009\&expectlist [last] = \", or \" ++ last\n\
 \\x0009\&\x0009\&expectlist (mid : rest) = \", \" ++ mid ++ expectlist rest\n\
 \\n\
 \\x0009\&\x0009\&messages [] = []\n\
 \\x0009\&\x0009\&messages (Expected exp : rest) = messages rest\n\
 \\x0009\&\x0009\&messages (Message msg : rest) =\n\
 \\x0009\&\x0009\&\x0009\&show pos ++ \": \" ++ msg ++ \"\\n\" ++ messages rest\n\
 \\n\
 \errorAnnotate :: Bool -> String -> Pos -> Result d v -> Result d v\n\
 \errorAnnotate isStrict desc pos = munge where\n\
 \    munge (Parsed v rem err) = Parsed v rem (fix err)\n\
 \    munge (NoParse err) = NoParse (fix err)\n\
 \    fix (err@(ParseError p ms)) =\n\
 \        if p > pos && not isStrict\n\
 \            then err else expError pos desc\n\
 \\n\
 \expError pos desc = ParseError pos [Expected desc]\n\
 \"



