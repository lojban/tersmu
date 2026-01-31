{-# LANGUAGE FlexibleInstances #-}
module JboTree where

import JboProp
import JboSyntax
import Logic hiding (Term)
import qualified Logic (Term)
import Data.List (intercalate)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

-- Graph representation for DAG output
data PropGraph = PropGraph
    { pgNodes :: [GraphNode]
    , pgEdges :: [GraphEdge]
    } deriving (Show, Eq)

data GraphNode = GraphNode
    { gnId :: String
    , gnType :: String
    , gnRule :: String
    , gnText :: String
    } deriving (Show, Eq)

data GraphEdge = GraphEdge
    { geSource :: String
    , geTarget :: String
    , geLabel :: String
    } deriving (Show, Eq)

-- | A serializable AST representation of JboProp
data PropTree
    = PTNot PropTree
    | PTConnected String PropTree PropTree
    | PTNonLogConnected String PropTree PropTree  -- Joik as string
    | PTQuantified QuantifierInfo (Maybe PropTree) PropTree
    | PTModal ModalInfo PropTree
    | PTRel RelInfo [TermTree]
    | PTEet
    | PTError String
    deriving (Show, Eq)

data QuantifierInfo = QuantifierInfo
    { qiQuantifier :: String    -- "∀", "∃", etc.
    , qiVariable   :: String    -- "x1", "x2", etc.
    } deriving (Show, Eq)

data ModalInfo = ModalInfo
    { miType :: String        -- "tagged", "veridical", "question", "event"
    , miTag  :: Maybe String  -- tag representation
    , miTerm :: Maybe TermTree
    } deriving (Show, Eq)

data RelInfo = RelInfo
    { riName :: String        -- predicate name or description
    , riType :: String        -- "brivla", "tanru", "equal", etc.
    } deriving (Show, Eq)

data TermTree
    = TTVar String            -- x1, x2, etc.
    | TTConstant String       -- c0, c1, etc.
    | TTNamed String          -- la djan.
    | TTUnfilled
    | TTJoiked String TermTree TermTree
    | TTQualified String TermTree
    | TTValue String          -- li ...
    | TTQuote String
    | TTComplex String        -- Fallback for complex terms
    deriving (Show, Eq, Ord)

-- | Convert a JboProp to a PropGraph (DAG format with shared nodes)
jboPropToGraph :: JboProp -> PropGraph
jboPropToGraph p = 
    let (rootId, st) = runState (convertPropToGraph p) (GraphState 0 [] [] Map.empty)
    in PropGraph (gsNodes st) (gsEdges st)

-- State for graph construction
data GraphState = GraphState
    { gsNextId :: Int
    , gsNodes :: [GraphNode]
    , gsEdges :: [GraphEdge]
    , gsTermCache :: Map.Map TermTree String  -- Cache for shared terms
    } deriving (Show)

type GraphM = State GraphState

newNodeId :: GraphM String
newNodeId = do
    st <- get
    let nid = gsNextId st
    put $ st { gsNextId = nid + 1 }
    return $ "n" ++ show nid

addNode :: String -> String -> String -> String -> GraphM ()
addNode nid typ rule txt = do
    st <- get
    put $ st { gsNodes = GraphNode nid typ rule txt : gsNodes st }

addEdge :: String -> String -> String -> GraphM ()
addEdge src tgt lbl = do
    st <- get
    put $ st { gsEdges = GraphEdge src tgt lbl : gsEdges st }

-- Convert proposition to graph, returning the root node ID
convertPropToGraph :: JboProp -> GraphM String
convertPropToGraph (Not p) = do
    nid <- newNodeId
    addNode nid "logic" "NA" "¬"
    childId <- convertPropToGraph p
    addEdge nid childId ""
    return nid

convertPropToGraph (Connected c p1 p2) = do
    nid <- newNodeId
    addNode nid "connective" "JOI" (show c)
    leftId <- convertPropToGraph p1
    rightId <- convertPropToGraph p2
    addEdge nid leftId "L"
    addEdge nid rightId "R"
    return nid

convertPropToGraph (NonLogConnected c p1 p2) = do
    nid <- newNodeId
    addNode nid "connective" "JOI" (show c)
    leftId <- convertPropToGraph p1
    rightId <- convertPropToGraph p2
    addEdge nid leftId "L"
    addEdge nid rightId "R"
    return nid

convertPropToGraph (Quantified q mr p) = do
    nid <- newNodeId
    n <- gets gsNextId
    let varName = "x_" ++ show n
    addNode nid "quantifier" "quant" (show q ++ " " ++ varName)
    case mr of
        Nothing -> return ()
        Just r -> do
            restrId <- convertPropToGraph (r n)
            addEdge nid restrId "restr"
    bodyId <- convertPropToGraph (p n)
    addEdge nid bodyId ""
    return nid

convertPropToGraph (Modal m p) = do
    nid <- newNodeId
    let (typ, tag, mterm) = case m of
            JboTagged tag mt -> ("modal", Just $ show tag, mt)
            WithEventAs t -> ("modal", Just "event", Just t)
            QTruthModal -> ("modal", Just "truth", Nothing)
            NonVeridical -> ("modal", Just "non-veridical", Nothing)
    addNode nid "modal" "BAI" (fromMaybe "" tag)
    case mterm of
        Just t -> do
            termId <- convertTermToGraph t
            addEdge nid termId "term"
        Nothing -> return ()
    childId <- convertPropToGraph p
    addEdge nid childId ""
    return nid

convertPropToGraph (Rel r ts) = do
    nid <- newNodeId
    let RelInfo name rtype = convertRel r
    addNode nid "relation" rtype name
    mapM_ (\(idx, t) -> do
        termId <- convertTermToGraph t
        addEdge nid termId ("x" ++ show (idx + 1))
        ) (zip [0..] ts)
    return nid

convertPropToGraph Eet = do
    nid <- newNodeId
    addNode nid "eet" "eet" ""
    return nid

-- Convert term to graph, with caching for shared references
convertTermToGraph :: JboTerm -> GraphM String
convertTermToGraph term = do
    let tt = evalState (convertTerm term) 1
    -- Check if we've already created a node for this term
    cache <- gets gsTermCache
    case Map.lookup tt cache of
        Just existingId -> return existingId  -- Reuse existing node
        Nothing -> do
            -- Create new node
            nid <- newNodeId
            let (typ, txt) = case tt of
                    TTVar s -> ("var", s)
                    TTConstant s -> ("constant", s)
                    TTNamed s -> ("named", s)
                    TTUnfilled -> ("unfilled", "")
                    TTValue s -> ("value", s)
                    TTQuote s -> ("quote", s)
                    TTComplex s -> ("complex", s)
                    TTJoiked j t1 t2 -> ("joiked", j)
                    TTQualified q t -> ("qualified", q)
            addNode nid typ "sumti" txt
            -- Cache this term
            st <- get
            put $ st { gsTermCache = Map.insert tt nid (gsTermCache st) }
            -- Handle composite terms
            case tt of
                TTJoiked _ t1 t2 -> do
                    id1 <- convertTermToGraph (termTreeToJboTerm t1)
                    id2 <- convertTermToGraph (termTreeToJboTerm t2)
                    addEdge nid id1 "1"
                    addEdge nid id2 "2"
                TTQualified _ t -> do
                    tid <- convertTermToGraph (termTreeToJboTerm t)
                    addEdge nid tid ""
                _ -> return ()
            return nid

-- Helper to convert TermTree back to JboTerm (simplified, lossy)
termTreeToJboTerm :: TermTree -> JboTerm
termTreeToJboTerm (TTVar s) = Var 0  -- Simplified
termTreeToJboTerm (TTConstant s) = Constant 0 []
termTreeToJboTerm (TTNamed s) = Named s
termTreeToJboTerm TTUnfilled = Unfilled
termTreeToJboTerm _ = Unfilled

-- Original tree conversion (kept for backward compatibility)
jboPropToTree :: JboProp -> PropTree
jboPropToTree p = evalState (convertProp p) 1

type ConvertM = State Int

convertProp :: JboProp -> ConvertM PropTree
convertProp (Not p) = PTNot <$> convertProp p
convertProp (Connected c p1 p2) = PTConnected (show c) <$> convertProp p1 <*> convertProp p2
convertProp (NonLogConnected c p1 p2) = PTNonLogConnected (show c) <$> convertProp p1 <*> convertProp p2
convertProp (Quantified q mr p) = do
    n <- get
    put (n + 1)
    let varName = "x_" ++ show n
    rTree <- case mr of
        Nothing -> return Nothing
        Just r -> Just <$> convertProp (r n)
    bodyTree <- convertProp (p n)
    return $ PTQuantified (QuantifierInfo (show q) varName) rTree bodyTree
convertProp (Modal m p) = do
    mi <- convertModal m
    pTree <- convertProp p
    return $ PTModal mi pTree
convertProp (Rel r ts) = do
    let ri = convertRel r
    tts <- mapM convertTerm ts
    return $ PTRel ri tts
convertProp Eet = return PTEet

convertModal :: JboModalOp -> ConvertM ModalInfo
convertModal (JboTagged tag mt) = do
    mtTree <- case mt of
        Nothing -> return Nothing
        Just t -> Just <$> convertTerm t
    return $ ModalInfo "tagged" (Just $ show tag) mtTree
convertModal (WithEventAs t) = do
    tTree <- convertTerm t
    return $ ModalInfo "event" Nothing (Just tTree)
convertModal QTruthModal = return $ ModalInfo "question" (Just "truth") Nothing
convertModal NonVeridical = return $ ModalInfo "veridical" (Just "non-veridical") Nothing

convertRel :: JboRel -> RelInfo
convertRel (Brivla s) = RelInfo s "brivla"
convertRel (Tanru _ _) = RelInfo "tanru" "tanru" -- Simplified
convertRel (TanruConnective _ _ _) = RelInfo "tanru-connective" "tanru"
convertRel (ScalarNegatedRel _ r) = let RelInfo n t = convertRel r in RelInfo ("not " ++ n) t
convertRel (AbsPred _ _) = RelInfo "abstraction" "abstraction"
convertRel (AbsProp _ _) = RelInfo "abstraction-prop" "abstraction"
convertRel (Moi _ _) = RelInfo "moi" "moi"
convertRel (Among _) = RelInfo "among" "among"
convertRel Equal = RelInfo "=" "equal"
convertRel (UnboundBribasti _) = RelInfo "unbound" "unbound"
convertRel (BoundRVar n) = RelInfo ("R_" ++ show n) "bound-var"
convertRel (RVar n) = RelInfo ("R_" ++ show n) "var"
convertRel (OperatorRel _) = RelInfo "operator" "operator"
convertRel (TagRel _) = RelInfo "tag" "tag"

convertTerm :: JboTerm -> ConvertM TermTree
convertTerm (BoundVar n) = return $ TTVar ("x_" ++ show n)
convertTerm (Var n) = return $ TTVar ("v_" ++ show n)
convertTerm (Constant n _) = return $ TTConstant ("c" ++ show n)
convertTerm (Named s) = return $ TTNamed s
convertTerm (PredNamed _) = return $ TTComplex "pred-named"
convertTerm (NonAnaph s) = return $ TTNamed s
convertTerm (UnboundSumbasti _) = return $ TTComplex "unbound-sumbasti"
convertTerm (JboQuote _) = return $ TTQuote "quote"
convertTerm (JboErrorQuote _) = return $ TTQuote "error-quote"
convertTerm (JboNonJboQuote s) = return $ TTQuote s
convertTerm (TheMex _) = return $ TTComplex "mex"
convertTerm (Value _) = return $ TTValue "value"
convertTerm (Valsi s) = return $ TTValue s
convertTerm Unfilled = return TTUnfilled
convertTerm (JoikedTerms j t1 t2) = do
    t1' <- convertTerm t1
    t2' <- convertTerm t2
    return $ TTJoiked (show j) t1' t2'
convertTerm (QualifiedTerm _ t) = do
    t' <- convertTerm t
    return $ TTQualified "qual" t'

-- Manual JSON Serialization

class JsonSerializable a where
    toJson :: a -> String

instance JsonSerializable PropGraph where
    toJson (PropGraph nodes edges) = 
        "{\"nodes\":[" ++ intercalate "," (map toJson (reverse nodes)) ++ 
        "],\"edges\":[" ++ intercalate "," (map toJson (reverse edges)) ++ "]}"

instance JsonSerializable GraphNode where
    toJson (GraphNode nid typ rule txt) =
        "{\"id\":\"" ++ jsonEscape nid ++ 
        "\",\"type\":\"" ++ jsonEscape typ ++ 
        "\",\"rule\":\"" ++ jsonEscape rule ++ 
        "\",\"text\":\"" ++ jsonEscape txt ++ "\"}"

instance JsonSerializable GraphEdge where
    toJson (GraphEdge src tgt lbl) =
        "{\"source\":\"" ++ jsonEscape src ++ 
        "\",\"target\":\"" ++ jsonEscape tgt ++ 
        "\",\"label\":\"" ++ jsonEscape lbl ++ "\"}"

instance JsonSerializable PropTree where
    toJson (PTNot p) = "{\"type\":\"not\",\"child\":" ++ toJson p ++ "}"
    toJson (PTConnected c p1 p2) = "{\"type\":\"connected\",\"connective\":\"" ++ jsonEscape c ++ "\",\"left\":" ++ toJson p1 ++ ",\"right\":" ++ toJson p2 ++ "}"
    toJson (PTNonLogConnected c p1 p2) = "{\"type\":\"non-log-connected\",\"connective\":\"" ++ jsonEscape c ++ "\",\"left\":" ++ toJson p1 ++ ",\"right\":" ++ toJson p2 ++ "}"
    toJson (PTQuantified qi r p) = "{\"type\":\"quantified\",\"quantifier\":" ++ toJson qi ++ ",\"restriction\":" ++ maybe "null" toJson r ++ ",\"child\":" ++ toJson p ++ "}"
    toJson (PTModal mi p) = "{\"type\":\"modal\",\"modal\":" ++ toJson mi ++ ",\"child\":" ++ toJson p ++ "}"
    toJson (PTRel ri ts) = "{\"type\":\"relation\",\"relation\":" ++ toJson ri ++ ",\"terms\":[" ++ intercalate "," (map toJson ts) ++ "]}"
    toJson PTEet = "{\"type\":\"eet\"}"
    toJson (PTError s) = "{\"type\":\"error\",\"message\":\"" ++ jsonEscape s ++ "\"}"

instance JsonSerializable QuantifierInfo where
    toJson (QuantifierInfo q v) = "{\"quantifier\":\"" ++ jsonEscape q ++ "\",\"variable\":\"" ++ jsonEscape v ++ "\"}"

instance JsonSerializable ModalInfo where
    toJson (ModalInfo t tag term) = "{\"type\":\"" ++ jsonEscape t ++ "\",\"tag\":" ++ maybe "null" (\s -> "\"" ++ jsonEscape s ++ "\"") tag ++ ",\"term\":" ++ maybe "null" toJson term ++ "}"

instance JsonSerializable RelInfo where
    toJson (RelInfo n t) = "{\"name\":\"" ++ jsonEscape n ++ "\",\"type\":\"" ++ jsonEscape t ++ "\"}"

instance JsonSerializable TermTree where
    toJson (TTVar s) = "{\"type\":\"var\",\"value\":\"" ++ jsonEscape s ++ "\"}"
    toJson (TTConstant s) = "{\"type\":\"constant\",\"value\":\"" ++ jsonEscape s ++ "\"}"
    toJson (TTNamed s) = "{\"type\":\"named\",\"value\":\"" ++ jsonEscape s ++ "\"}"
    toJson TTUnfilled = "{\"type\":\"unfilled\"}"
    toJson (TTJoiked j t1 t2) = "{\"type\":\"joiked\",\"joik\":\"" ++ jsonEscape j ++ "\",\"term1\":" ++ toJson t1 ++ ",\"term2\":" ++ toJson t2 ++ "}"
    toJson (TTQualified q t) = "{\"type\":\"qualified\",\"qualifier\":\"" ++ jsonEscape q ++ "\",\"term\":" ++ toJson t ++ "\"}"
    toJson (TTValue s) = "{\"type\":\"value\",\"value\":\"" ++ jsonEscape s ++ "\"}"
    toJson (TTQuote s) = "{\"type\":\"quote\",\"value\":\"" ++ jsonEscape s ++ "\"}"
    toJson (TTComplex s) = "{\"type\":\"complex\",\"value\":\"" ++ jsonEscape s ++ "\"}"

-- JSON helper
jsonEscape :: String -> String
jsonEscape = concatMap $ \c -> case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> [c]
