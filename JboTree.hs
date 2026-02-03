{-# LANGUAGE FlexibleInstances #-}
module JboTree where

import JboProp
import JboSyntax
import Logic hiding (Term)
import qualified Logic (Term)
import Data.List (intercalate)
import Control.Monad.State
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Char (ord)
import Data.Bits (xor)

-- | Graph-based output representation
data GraphOutput = GraphOutput
    { goNodes :: [GraphNode]
    , goEdges :: [GraphEdge]
    } deriving (Show, Eq)

data GraphNode = GraphNode
    { gnId :: String
    , gnType :: String
    , gnData :: NodeData
    } deriving (Show, Eq)

data NodeData
    = NDRelation { ndRelName :: String, ndRelType :: String, ndSelmaho :: Maybe String }
    | NDTerm { ndTermValue :: String, ndTermType :: String, ndSelmaho :: Maybe String }
    | NDQuantifier { ndQuantifier :: String, ndVariable :: String, ndSelmaho :: Maybe String }
    | NDModal { ndModalType :: String, ndModalTag :: Maybe String, ndSelmaho :: Maybe String }
    | NDConnective { ndConnective :: String, ndSelmaho :: Maybe String }
    | NDNot { ndSelmaho :: Maybe String }
    | NDSideTexticule { ndSideType :: String, ndSideContent :: String }
    | NDEet
    | NDError { ndErrorMsg :: String }
    deriving (Show, Eq)

data GraphEdge = GraphEdge
    { geSource :: String
    , geTarget :: String
    , geLabel :: String
    } deriving (Show, Eq)

-- | State for graph conversion
data GraphState = GraphState
    { gsNodeMap :: Map.Map String String  -- content hash -> node ID
    , gsNodes :: [GraphNode]
    , gsEdges :: [GraphEdge]
    , gsNextId :: Int
    }

type GraphM = State GraphState

-- | Convert JboProp to GraphOutput
jboPropToGraph :: JboProp -> GraphOutput
jboPropToGraph p = jboPropsToGraph [p]

-- | Convert multiple JboProps to a single GraphOutput with shared nodes
jboPropsToGraph :: [JboProp] -> GraphOutput
jboPropsToGraph ps = 
    let initState = GraphState Map.empty [] [] 0
        (_, finalState) = runState (mapM_ (\p -> convertPropToGraph p Nothing) ps) initState
    in GraphOutput (reverse $ gsNodes finalState) (reverse $ gsEdges finalState)

-- | Convert JboText (including side texticules) to GraphOutput
-- Side texticules are attached to the next main proposition node they precede
jboTextToGraph :: JboText -> GraphOutput
jboTextToGraph texticules =
    let initState = GraphState Map.empty [] [] 0
        (_, finalState) = runState (processTexticules texticules []) initState
    in GraphOutput (reverse $ gsNodes finalState) (reverse $ gsEdges finalState)
  where
    processTexticules :: [Texticule] -> [(SideType, Texticule)] -> GraphM ()
    processTexticules [] pendingSides = return ()  -- Ignore orphaned side texticules
    processTexticules (t:ts) pendingSides = do
        case t of
            TexticuleProp p -> do
                -- Create the main proposition node
                propId <- convertPropToGraph p Nothing
                -- Attach any pending side texticules to this proposition
                mapM_ (\(sideType, sideT) -> do
                    sideId <- convertSideTexticuleToNode sideType sideT
                    addEdge propId sideId "side") pendingSides
                -- Continue with empty pending list
                processTexticules ts []
            TexticuleFrag f -> do
                -- Fragments are rare, add to pending
                processTexticules ts (pendingSides ++ [(SideBracketed, TexticuleFrag f)])
            TexticuleSide sideType innerTexticule -> do
                -- Add to pending side texticules
                processTexticules ts (pendingSides ++ [(sideType, innerTexticule)])
    
    convertSideTexticuleToNode :: SideType -> Texticule -> GraphM String
    convertSideTexticuleToNode sideType (TexticuleProp p) = do
        -- Convert the proposition to a string representation for the side node
        let sideTypeStr = case sideType of
                SideBracketed -> "TO"
                SideDiscursive -> "SEI"
        let contentKey = "side:" ++ sideTypeStr ++ ":" ++ show p
        getOrCreateNode contentKey "side-texticule" (NDSideTexticule sideTypeStr (show p))
    convertSideTexticuleToNode sideType (TexticuleFrag f) = do
        let sideTypeStr = case sideType of
                SideBracketed -> "TO"
                SideDiscursive -> "SEI"
        convertFragToSideNode sideTypeStr f
    convertSideTexticuleToNode sideType (TexticuleSide _ innerT) = 
        -- Nested side texticules - unwrap
        convertSideTexticuleToNode sideType innerT
    
    convertFragToSideNode :: String -> JboFragment -> GraphM String
    convertFragToSideNode sideTypeStr (JboFragTerms ts) = do
        let contentKey = "frag:" ++ sideTypeStr ++ ":" ++ show (length ts)
        getOrCreateNode contentKey "side-texticule" (NDSideTexticule sideTypeStr "fragment")
    convertFragToSideNode sideTypeStr (JboFragUnparsed _) = do
        let contentKey = "frag:" ++ sideTypeStr ++ ":unparsed"
        getOrCreateNode contentKey "side-texticule" (NDSideTexticule sideTypeStr "unparsed")

-- | Simple string hash function (FNV-1a algorithm)
simpleHash :: String -> Int
simpleHash = foldl (\h c -> (h * 16777619) `xor` ord c) 2166136261

-- | Generate a simple hash for content-based deduplication
contentHash :: String -> String
contentHash s = "h" ++ show (abs $ simpleHash s)

-- | Create or reuse a node based on content
getOrCreateNode :: String -> String -> NodeData -> GraphM String
getOrCreateNode contentKey nodeType nodeData = do
    st <- get
    case Map.lookup contentKey (gsNodeMap st) of
        Just nodeId -> return nodeId  -- Reuse existing node
        Nothing -> do
            let nodeId = "n" ++ show (gsNextId st)
            let node = GraphNode nodeId nodeType nodeData
            put $ st { gsNodeMap = Map.insert contentKey nodeId (gsNodeMap st)
                     , gsNodes = node : gsNodes st
                     , gsNextId = gsNextId st + 1
                     }
            return nodeId

-- | Add an edge
addEdge :: String -> String -> String -> GraphM ()
addEdge source target label = do
    st <- get
    let edge = GraphEdge source target label
    put $ st { gsEdges = edge : gsEdges st }

-- | Convert PropTree to graph (recursive)
convertPropToGraph :: JboProp -> Maybe String -> GraphM String
convertPropToGraph (Not p) parentId = do
    let contentKey = "not"
    nodeId <- getOrCreateNode contentKey "not" (NDNot (Just "NA"))
    childId <- convertPropToGraph p (Just nodeId)
    addEdge nodeId childId ""
    maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
    return nodeId

convertPropToGraph (Connected c p1 p2) parentId = do
    let connStr = show c
    let contentKey = "conn:" ++ connStr
    nodeId <- getOrCreateNode contentKey "connective" (NDConnective connStr (Just "JOI"))
    leftId <- convertPropToGraph p1 (Just nodeId)
    rightId <- convertPropToGraph p2 (Just nodeId)
    addEdge nodeId leftId "L"
    addEdge nodeId rightId "R"
    maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
    return nodeId

convertPropToGraph (NonLogConnected c p1 p2) parentId = do
    let connStr = show c
    let contentKey = "nlconn:" ++ connStr
    nodeId <- getOrCreateNode contentKey "non-log-connective" (NDConnective connStr (Just "JOI"))
    leftId <- convertPropToGraph p1 (Just nodeId)
    rightId <- convertPropToGraph p2 (Just nodeId)
    addEdge nodeId leftId "L"
    addEdge nodeId rightId "R"
    maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
    return nodeId

convertPropToGraph (Quantified q mr p) parentId = do
    st <- get
    let n = gsNextId st
    let varName = "x_" ++ show n
    let qStr = show q
    let contentKey = "quant:" ++ qStr ++ ":" ++ varName
    nodeId <- getOrCreateNode contentKey "quantifier" (NDQuantifier qStr varName (Just "PA"))
    
    case mr of
        Just r -> do
            restrId <- convertPropToGraph (r n) (Just nodeId)
            addEdge nodeId restrId "restr"
        Nothing -> return ()
    
    bodyId <- convertPropToGraph (p n) (Just nodeId)
    addEdge nodeId bodyId ""
    maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
    return nodeId

convertPropToGraph (Modal m p) parentId = do
    (modalType, modalTag, mTermId, selmaho) <- convertModal m
    let contentKey = "modal:" ++ modalType ++ ":" ++ show modalTag
    nodeId <- getOrCreateNode contentKey "modal" (NDModal modalType modalTag selmaho)
    
    case mTermId of
        Just termId -> addEdge nodeId termId "term"
        Nothing -> return ()
    
    childId <- convertPropToGraph p (Just nodeId)
    addEdge nodeId childId ""
    maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
    return nodeId

convertPropToGraph (Rel r ts) parentId = do
    case r of
        -- Special handling for abstractions - they contain nested propositions
        AbsPred abstractor (JboNPred arity pred) -> do
            let contentKey = "abs:" ++ show abstractor
            nodeId <- getOrCreateNode contentKey "abstraction" (NDRelation (show abstractor) "abstraction" (Just "NU"))
            
            -- Convert the abstracted proposition with dummy arguments
            let dummyArgs = replicate arity Unfilled
            let innerProp = pred dummyArgs
            innerPropId <- convertPropToGraph innerProp (Just nodeId)
            addEdge nodeId innerPropId "body"
            
            -- Convert terms (the constant that this abstraction binds to)
            termIds <- mapM convertTermToGraph ts
            sequence_ $ zipWith (\termId idx -> addEdge nodeId termId ("x" ++ show idx)) termIds [1..]
            
            maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
            return nodeId
            
        AbsProp abstractor innerProp -> do
            let contentKey = "absprop:" ++ show abstractor
            nodeId <- getOrCreateNode contentKey "abstraction-prop" (NDRelation (show abstractor) "abstraction" (Just "NU"))
            
            -- Convert the abstracted proposition
            innerPropId <- convertPropToGraph innerProp (Just nodeId)
            addEdge nodeId innerPropId "body"
            
            -- Convert terms
            termIds <- mapM convertTermToGraph ts
            sequence_ $ zipWith (\termId idx -> addEdge nodeId termId ("x" ++ show idx)) termIds [1..]
            
            maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
            return nodeId
            
        -- Regular relations
        _ -> do
            let (relName, relType, selmaho) = convertRel r
            let contentKey = "rel:" ++ relType ++ ":" ++ relName
            nodeId <- getOrCreateNode contentKey "relation" (NDRelation relName relType selmaho)
            
            termIds <- mapM convertTermToGraph ts
            sequence_ $ zipWith (\termId idx -> addEdge nodeId termId ("x" ++ show idx)) termIds [1..]
            
            maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
            return nodeId

convertPropToGraph Eet parentId = do
    let contentKey = "eet"
    nodeId <- getOrCreateNode contentKey "eet" NDEet
    maybe (return ()) (\pid -> addEdge pid nodeId "") parentId
    return nodeId

-- | Convert modal operator
convertModal :: JboModalOp -> GraphM (String, Maybe String, Maybe String, Maybe String)
convertModal (JboTagged tag mt) = do
    termId <- case mt of
        Nothing -> return Nothing
        Just t -> Just <$> convertTermToGraph t
    
    -- Extract potential selmaho from tag
    let selmaho = getSelmahoFromTag tag
    
    return ("tagged", Just $ show tag, termId, selmaho)
convertModal (WithEventAs t) = do
    termId <- convertTermToGraph t
    return ("event", Nothing, Just termId, Just "NOI")
convertModal QTruthModal = return ("question", Just "truth", Nothing, Just "UI")
convertModal NonVeridical = return ("veridical", Just "non-veridical", Nothing, Just "LE")

-- | Convert relation
convertRel :: JboRel -> (String, String, Maybe String)
convertRel (Brivla s) = (s, "brivla", Just "BRIVLA")
convertRel (Tanru _ _) = ("tanru", "tanru", Just "BRIVLA")
convertRel (TanruConnective _ _ _) = ("tanru-connective", "tanru", Just "JOI")
convertRel (ScalarNegatedRel _ r) = let (n, t, s) = convertRel r in ("not " ++ n, t, s)
convertRel (AbsPred _ _) = ("abstraction", "abstraction", Just "NU")
convertRel (AbsProp _ _) = ("abstraction-prop", "abstraction", Just "NU")
convertRel (Moi _ _) = ("moi", "moi", Just "MOI")
convertRel (Among _) = ("among", "among", Nothing)
convertRel Equal = ("=", "equal", Nothing)
convertRel (UnboundBribasti _) = ("unbound", "unbound", Nothing)
convertRel (BoundRVar n) = ("R_" ++ show n, "bound-var", Just "KOhA")
convertRel (RVar n) = ("R_" ++ show n, "var", Just "KOhA")
convertRel (OperatorRel _) = ("operator", "operator", Nothing)
convertRel (TagRel tag) = ("tag", "tag", getSelmahoFromTag tag)

-- | Extract selmaho from a tag
getSelmahoFromTag :: JboTag -> Maybe String
getSelmahoFromTag (DecoratedTagUnits units) = 
    case mapMaybe getSelmahoFromDTU units of
        [] -> Nothing
        (s:_) -> Just s -- Return the first one for now
getSelmahoFromTag (ConnectedTag _ t1 t2) = 
    case (getSelmahoFromTag t1, getSelmahoFromTag t2) of
        (Just s1, _) -> Just s1
        (_, Just s2) -> Just s2
        _ -> Nothing

getSelmahoFromDTU :: DecoratedAbsTagUnit r t -> Maybe String
getSelmahoFromDTU dtu = getSelmahoFromTagUnit (tagUnit dtu)

getSelmahoFromTagUnit :: AbsTagUnit r t -> Maybe String
getSelmahoFromTagUnit (TenseCmavo c) = lookupSelmaho c
getSelmahoFromTagUnit (CAhA _) = Just "CAhA"
getSelmahoFromTagUnit (FAhA _ _) = Just "FAhA"
getSelmahoFromTagUnit (ROI _ _ _) = Just "ROI"
getSelmahoFromTagUnit (TAhE_ZAhO _ c) = lookupSelmaho c -- Could be TAhE or ZAhO
getSelmahoFromTagUnit (BAI _) = Just "BAI"
getSelmahoFromTagUnit (FIhO _) = Just "FIhO"
getSelmahoFromTagUnit (CUhE _) = Just "CUhE"
getSelmahoFromTagUnit KI = Just "KI"

lookupSelmaho :: String -> Maybe String
lookupSelmaho c 
    | c `elem` ["pu", "ca", "ba"] = Just "PU"
    | c `elem` ["zi", "za", "zu"] = Just "ZI"
    | c `elem` ["ze'i", "ze'a", "ze'u"] = Just "ZEhA"
    | c `elem` ["vi", "va", "vu"] = Just "VA"
    | c `elem` ["ve'i", "ve'a", "ve'u"] = Just "VEhA"
    | c `elem` ["vi'i", "vi'a", "vi'u"] = Just "VIhA"
    | c `elem` ["fa", "fe", "fi", "fo", "fu"] = Just "FA"
    | c `elem` ["co'i", "co'a", "co'u", "mo'u", "za'o", "de'a", "di'a"] = Just "ZAhO" -- incomplete list, illustrative
    | otherwise = Nothing -- Fallback if not found

-- | Convert term to graph
convertTermToGraph :: JboTerm -> GraphM String
convertTermToGraph term = do
    -- Generate a robust key based on content
    -- Default key uses show, but we handle Quote specially due to dummy Show instance
    let defaultKey = "term:" ++ contentHash (show term)
    
    case term of
        JoikedTerms j t1 t2 -> do
            let jStr = show j
            nodeId <- getOrCreateNode defaultKey "term" (NDTerm ("joiked:" ++ jStr) "joiked" (Just "JOI"))
            t1Id <- convertTermToGraph t1
            t2Id <- convertTermToGraph t2
            addEdge nodeId t1Id "t1"
            addEdge nodeId t2Id "t2"
            return nodeId
            
        QualifiedTerm q t -> do
            nodeId <- getOrCreateNode defaultKey "term" (NDTerm "qualified" "qualified" (Just "LAhE"))
            tId <- convertTermToGraph t
            addEdge nodeId tId "term"
            return nodeId
            
        Constant n ts -> do
            let v = "c" ++ show n
            -- Key based on "c<n>" is sufficient if index implies identity
            -- If arguments matter for identity, defaultKey handles it
            nodeId <- getOrCreateNode defaultKey "term" (NDTerm v "constant" (Just "KOhA"))
            termIds <- mapM convertTermToGraph ts
            sequence_ $ zipWith (\tid idx -> addEdge nodeId tid ("arg" ++ show idx)) termIds [1..]
            return nodeId

        JboQuote (ParsedQuote text) -> do
            -- Special handling for Quote to avoid collision due to dummy Show instance
            let key = "term:quote:" ++ contentHash (show text)
            let (val, typ, _, sel) = convertTerm term
            getOrCreateNode key "term" (NDTerm val typ sel)

        JboNonJboQuote s -> do
             let key = "term:quote:" ++ contentHash s
             let (val, typ, _, sel) = convertTerm term
             getOrCreateNode key "term" (NDTerm val typ sel)

        _ -> do
            let (val, typ, _, sel) = convertTerm term
            getOrCreateNode defaultKey "term" (NDTerm val typ sel)

-- | Convert term
convertTerm :: JboTerm -> (String, String, String, Maybe String)
convertTerm (BoundVar n) = 
    let v = "x_" ++ show n 
    in (v, "var", "term:var:" ++ v, Just "KOhA")
convertTerm (Var n) = 
    let v = "v_" ++ show n 
    in (v, "var", "term:var:" ++ v, Just "KOhA")
convertTerm (Constant n _) = 
    let v = "c" ++ show n 
    in (v, "constant", "term:constant:" ++ v, Just "KOhA")
convertTerm (Named s) = (s, "named", "term:named:" ++ s, Just "KOhA")
convertTerm (PredNamed _) = ("pred-named", "complex", "term:complex:pred-named", Nothing)
convertTerm (NonAnaph s) = (s, "named", "term:named:" ++ s, Just "KOhA")
convertTerm (UnboundSumbasti _) = ("unbound-sumbasti", "complex", "term:complex:unbound-sumbasti", Nothing)
convertTerm (JboQuote _) = ("quote", "quote", "term:quote:quote", Just "LU")
convertTerm (JboErrorQuote _) = ("error-quote", "quote", "term:quote:error-quote", Just "LU")
convertTerm (JboNonJboQuote s) = (s, "quote", "term:quote:" ++ s, Just "ZO")
convertTerm (TheMex _) = ("mex", "complex", "term:complex:mex", Nothing)
convertTerm (Value _) = ("value", "value", "term:value:value", Just "LI")
convertTerm (Valsi s) = (s, "value", "term:value:" ++ s, Just "BRIVLA")
convertTerm Unfilled = ("unfilled", "unfilled", "term:unfilled", Nothing)
convertTerm (JoikedTerms j t1 t2) = 
    let jStr = show j
    in ("joiked:" ++ jStr, "joiked", "term:joiked:" ++ jStr, Just "JOI")
convertTerm (QualifiedTerm _ t) = ("qualified", "qualified", "term:qualified", Just "LAhE")

-- JSON Serialization for GraphOutput

class JsonSerializable a where
    toJson :: a -> String

instance JsonSerializable GraphOutput where
    toJson (GraphOutput nodes edges) = 
        "{\"format\":\"graph\",\"nodes\":[" ++ intercalate "," (map toJson nodes) ++ 
        "],\"edges\":[" ++ intercalate "," (map toJson edges) ++ "]}"

instance JsonSerializable GraphNode where
    toJson (GraphNode nodeId nodeType nodeData) =
        "{\"id\":\"" ++ jsonEscape nodeId ++ 
        "\",\"type\":\"" ++ jsonEscape nodeType ++ 
        "\",\"data\":" ++ toJson nodeData ++ "}"

instance JsonSerializable NodeData where
    toJson (NDRelation name typ sel) = 
        "{\"name\":\"" ++ jsonEscape name ++ "\",\"relType\":\"" ++ jsonEscape typ ++ "\"" ++
        maybe "" (\s -> ",\"selmaho\":\"" ++ jsonEscape s ++ "\"") sel ++ "}"
    toJson (NDTerm value typ sel) = 
        "{\"value\":\"" ++ jsonEscape value ++ "\",\"termType\":\"" ++ jsonEscape typ ++ "\"" ++
        maybe "" (\s -> ",\"selmaho\":\"" ++ jsonEscape s ++ "\"") sel ++ "}"
    toJson (NDQuantifier q v sel) = 
        "{\"quantifier\":\"" ++ jsonEscape q ++ "\",\"variable\":\"" ++ jsonEscape v ++ "\"" ++
        maybe "" (\s -> ",\"selmaho\":\"" ++ jsonEscape s ++ "\"") sel ++ "}"
    toJson (NDModal typ tag sel) = 
        "{\"modalType\":\"" ++ jsonEscape typ ++ "\",\"tag\":" ++ 
        maybe "null" (\t -> "\"" ++ jsonEscape t ++ "\"") tag ++
        maybe "" (\s -> ",\"selmaho\":\"" ++ jsonEscape s ++ "\"") sel ++ "}"
    toJson (NDConnective c sel) = 
        "{\"connective\":\"" ++ jsonEscape c ++ "\"" ++
        maybe "" (\s -> ",\"selmaho\":\"" ++ jsonEscape s ++ "\"") sel ++ "}"
    toJson (NDNot sel) = "{" ++ maybe "" (\s -> "\"selmaho\":\"" ++ jsonEscape s ++ "\"") sel ++ "}"
    toJson (NDSideTexticule sideType content) =
        "{\"sideType\":\"" ++ jsonEscape sideType ++ "\",\"content\":\"" ++ jsonEscape content ++ "\"}"
    toJson NDEet = "{}"
    toJson (NDError msg) = 
        "{\"message\":\"" ++ jsonEscape msg ++ "\"}"

instance JsonSerializable GraphEdge where
    toJson (GraphEdge source target label) =
        "{\"source\":\"" ++ jsonEscape source ++ 
        "\",\"target\":\"" ++ jsonEscape target ++ 
        "\",\"label\":\"" ++ jsonEscape label ++ "\"}"

-- JSON helper
jsonEscape :: String -> String
jsonEscape = concatMap $ \c -> case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> [c]
