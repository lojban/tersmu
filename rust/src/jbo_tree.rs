//! Graph-based output from [JboTree.hs](../JboTree.hs).
//!
//! Ports the proposition-to-graph conversion used by the Haskell JSON graph output.

use crate::jbo_prop::{DecoratedTagUnit, JboModalOp, JboQuantifier, JboRel, JboTag, JboTagUnit, JboTerm, Texticule};
use crate::logic::Prop;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub type JboProp = Prop<JboRel, JboTerm, String, JboModalOp, JboQuantifier>;

// Ported from: JboTree.hs :: GraphOutput
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphOutput {
    pub format: String,
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

// Ported from: JboTree.hs :: GraphNode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphNode {
    pub id: String,
    #[serde(rename = "type")]
    pub node_type: String,
    pub data: NodeData,
}

// Ported from: JboTree.hs :: NodeData
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum NodeData {
    #[serde(rename = "relation")]
    Relation {
        name: String,
        #[serde(rename = "type")]
        rel_type: String,
        selmaho: Option<String>,
    },
    #[serde(rename = "term")]
    Term {
        value: String,
        #[serde(rename = "type")]
        term_type: String,
        selmaho: Option<String>,
    },
    #[serde(rename = "quantifier")]
    Quantifier {
        quantifier: String,
        variable: String,
        selmaho: Option<String>,
    },
    #[serde(rename = "modal")]
    Modal {
        modal_type: String,
        tag: Option<String>,
        selmaho: Option<String>,
    },
    #[serde(rename = "connective")]
    Connective {
        connective: String,
        selmaho: Option<String>,
    },
    #[serde(rename = "not")]
    Not {
        selmaho: Option<String>,
    },
    #[serde(rename = "side")]
    SideTexticule {
        side_type: String,
        content: String,
    },
    #[serde(rename = "eet")]
    Eet,
    #[serde(rename = "error")]
    Error {
        message: String,
    },
}

// Ported from: JboTree.hs :: GraphEdge
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphEdge {
    pub source: String,
    pub target: String,
    pub label: String,
}

// Ported from: JboTree.hs :: GraphState
struct GraphState {
    node_map: HashMap<String, String>,
    nodes: Vec<GraphNode>,
    edges: Vec<GraphEdge>,
    next_id: usize,
}

impl GraphState {
    fn new() -> Self {
        GraphState {
            node_map: HashMap::new(),
            nodes: Vec::new(),
            edges: Vec::new(),
            next_id: 0,
        }
    }
}

// Ported from: JboTree.hs :: simpleHash
fn simple_hash(s: &str) -> i32 {
    let mut h: i32 = 2166136261u32 as i32;
    for c in s.chars() {
        h = h.wrapping_mul(16777619).wrapping_add(c as i32);
    }
    h
}

// Ported from: JboTree.hs :: contentHash
fn content_hash(s: &str) -> String {
    format!("h{}", simple_hash(s).abs())
}

// Ported from: JboTree.hs :: getOrCreateNode
fn get_or_create_node(
    state: &mut GraphState,
    content_key: &str,
    node_type: &str,
    node_data: NodeData,
) -> String {
    if let Some(node_id) = state.node_map.get(content_key) {
        return node_id.clone();
    }

    let node_id = format!("n{}", state.next_id);
    state.next_id += 1;

    state.nodes.push(GraphNode {
        id: node_id.clone(),
        node_type: node_type.to_string(),
        data: node_data,
    });
    state.node_map.insert(content_key.to_string(), node_id.clone());

    node_id
}

// Ported from: JboTree.hs :: addEdge
fn add_edge(state: &mut GraphState, source: String, target: String, label: String) {
    state.edges.push(GraphEdge { source, target, label });
}

// Ported from: JboTree.hs :: convertPropToGraphWithLastTerm / convertPropToGraph
fn convert_prop_to_graph_with_last_term(
    state: &mut GraphState,
    prop: &JboProp,
    parent_id: Option<String>,
) -> (String, Option<String>) {
    match prop {
        Prop::Not(p) => {
            let node_id = get_or_create_node(
                state,
                "not",
                "not",
                NodeData::Not { selmaho: Some("NA".to_string()) },
            );
            let (child_id, _) = convert_prop_to_graph_with_last_term(state, p, Some(node_id.clone()));
            add_edge(state, node_id.clone(), child_id, String::new());
            add_parent_edge(state, parent_id, &node_id);
            (node_id, None)
        }
        Prop::Connected(c, p1, p2) => {
            let conn = c.to_string();
            let node_id = get_or_create_node(
                state,
                &format!("conn:{conn}"),
                "connective",
                NodeData::Connective { connective: conn, selmaho: Some("JOI".to_string()) },
            );
            let (left_id, _) = convert_prop_to_graph_with_last_term(state, p1, Some(node_id.clone()));
            let (right_id, last_term) = convert_prop_to_graph_with_last_term(state, p2, Some(node_id.clone()));
            add_edge(state, node_id.clone(), left_id, "L".to_string());
            add_edge(state, node_id.clone(), right_id, "R".to_string());
            add_parent_edge(state, parent_id, &node_id);
            (node_id, last_term)
        }
        Prop::NonLogConnected(c, p1, p2) => {
            let conn = c.clone();
            let node_id = get_or_create_node(
                state,
                &format!("nlconn:{conn}"),
                "non-log-connective",
                NodeData::Connective { connective: conn, selmaho: Some("JOI".to_string()) },
            );
            let (left_id, _) = convert_prop_to_graph_with_last_term(state, p1, Some(node_id.clone()));
            let (right_id, last_term) = convert_prop_to_graph_with_last_term(state, p2, Some(node_id.clone()));
            add_edge(state, node_id.clone(), left_id, "L".to_string());
            add_edge(state, node_id.clone(), right_id, "R".to_string());
            add_parent_edge(state, parent_id, &node_id);
            (node_id, last_term)
        }
        Prop::Quantified(q, restriction, body) => {
            let n = state.next_id as i32;
            let var_name = format!("x_{n}");
            let q_name = convert_quantifier(q);
            let node_id = get_or_create_node(
                state,
                &format!("quant:{q_name}:{var_name}"),
                "quantifier",
                NodeData::Quantifier {
                    quantifier: q_name,
                    variable: var_name,
                    selmaho: Some("PA".to_string()),
                },
            );
            if let Some(restriction) = restriction {
                let restriction_prop = restriction(n);
                let (restriction_id, _) = convert_prop_to_graph_with_last_term(state, &restriction_prop, Some(node_id.clone()));
                add_edge(state, node_id.clone(), restriction_id, "restr".to_string());
            }
            let body_prop = body(n);
            let (body_id, last_term) = convert_prop_to_graph_with_last_term(state, &body_prop, Some(node_id.clone()));
            add_edge(state, node_id.clone(), body_id, String::new());
            add_parent_edge(state, parent_id, &node_id);
            (node_id, last_term)
        }
        Prop::Modal(modal, p) => {
            let (modal_type, tag, term_id, selmaho) = convert_modal(state, modal);
            let node_id = get_or_create_node(
                state,
                &format!("modal:{modal_type}:{tag:?}"),
                "modal",
                NodeData::Modal { modal_type, tag, selmaho },
            );
            if let Some(term_id) = term_id {
                add_edge(state, node_id.clone(), term_id, "term".to_string());
            }
            let (child_id, last_term) = convert_prop_to_graph_with_last_term(state, p, Some(node_id.clone()));
            add_edge(state, node_id.clone(), child_id, String::new());
            add_parent_edge(state, parent_id, &node_id);
            (node_id, last_term)
        }
        Prop::Rel(rel, terms) => {
            let (node_id, last_term) = convert_rel_prop_to_graph(state, rel, terms, parent_id);
            (node_id, last_term)
        }
        Prop::Eet => {
            let node_id = get_or_create_node(state, "eet", "eet", NodeData::Eet);
            add_parent_edge(state, parent_id, &node_id);
            (node_id, None)
        }
    }
}

fn convert_prop_to_graph(state: &mut GraphState, prop: &JboProp, parent_id: Option<String>) -> String {
    convert_prop_to_graph_with_last_term(state, prop, parent_id).0
}

fn add_parent_edge(state: &mut GraphState, parent_id: Option<String>, node_id: &str) {
    if let Some(parent_id) = parent_id {
        add_edge(state, parent_id, node_id.to_string(), String::new());
    }
}

fn convert_rel_prop_to_graph(
    state: &mut GraphState,
    rel: &JboRel,
    terms: &[JboTerm],
    parent_id: Option<String>,
) -> (String, Option<String>) {
    match rel {
        JboRel::AbsPred(abstractor, pred) => {
            let node_id = get_or_create_node(
                state,
                &format!("abs:{abstractor}"),
                "abstraction",
                NodeData::Relation {
                    name: abstractor.clone(),
                    rel_type: "abstraction".to_string(),
                    selmaho: Some("NU".to_string()),
                },
            );
            let dummy_args = vec![JboTerm::Unfilled; pred.arity];
            let inner_prop = (pred.pred)(&dummy_args);
            let (inner_id, _) = convert_prop_to_graph_with_last_term(state, &inner_prop, Some(node_id.clone()));
            add_edge(state, node_id.clone(), inner_id, "body".to_string());
            let last_term = add_term_edges(state, &node_id, terms, "x");
            add_parent_edge(state, parent_id, &node_id);
            (node_id, last_term)
        }
        JboRel::AbsProp(abstractor, inner_prop) => {
            let node_id = get_or_create_node(
                state,
                &format!("absprop:{abstractor}"),
                "abstraction-prop",
                NodeData::Relation {
                    name: abstractor.clone(),
                    rel_type: "abstraction".to_string(),
                    selmaho: Some("NU".to_string()),
                },
            );
            let (inner_id, _) = convert_prop_to_graph_with_last_term(state, inner_prop, Some(node_id.clone()));
            add_edge(state, node_id.clone(), inner_id, "body".to_string());
            let last_term = add_term_edges(state, &node_id, terms, "x");
            add_parent_edge(state, parent_id, &node_id);
            (node_id, last_term)
        }
        _ => {
            let (name, rel_type, selmaho) = convert_rel(rel);
            let node_id = get_or_create_node(
                state,
                &format!("rel:{rel_type}:{name}"),
                "relation",
                NodeData::Relation { name, rel_type, selmaho },
            );
            let last_term = add_term_edges(state, &node_id, terms, "x");
            add_parent_edge(state, parent_id, &node_id);
            (node_id, last_term)
        }
    }
}

fn add_term_edges(state: &mut GraphState, node_id: &str, terms: &[JboTerm], prefix: &str) -> Option<String> {
    let mut last = None;
    for (idx, term) in terms.iter().enumerate() {
        let term_id = convert_term_to_graph(state, term);
        add_edge(state, node_id.to_string(), term_id.clone(), format!("{prefix}{}", idx + 1));
        last = Some(term_id);
    }
    last
}

// Ported from: JboTree.hs :: convertModal
fn convert_modal(state: &mut GraphState, modal: &JboModalOp) -> (String, Option<String>, Option<String>, Option<String>) {
    match modal {
        JboModalOp::Tagged(tag, term) => {
            let term_id = term.as_ref().map(|term| convert_term_to_graph(state, term));
            ("tagged".to_string(), Some(crate::jbo_show::jboshow_tag(tag)), term_id, get_selmaho_from_tag(tag))
        }
        JboModalOp::WithEventAs(term) => {
            let term_id = convert_term_to_graph(state, term);
            ("event".to_string(), None, Some(term_id), Some("NOI".to_string()))
        }
        JboModalOp::QTruthModal => ("question".to_string(), Some("truth".to_string()), None, Some("UI".to_string())),
        JboModalOp::NonVeridical => ("veridical".to_string(), Some("non-veridical".to_string()), None, Some("LE".to_string())),
    }
}

// Ported from: JboTree.hs :: convertRel
fn convert_rel(rel: &JboRel) -> (String, String, Option<String>) {
    match rel {
        JboRel::Brivla(s) => (s.clone(), "brivla".to_string(), Some("BRIVLA".to_string())),
        JboRel::Equal => ("=".to_string(), "equal".to_string(), None),
        JboRel::Among(_) => ("among".to_string(), "among".to_string(), None),
        JboRel::Tanru(_, _) | JboRel::AppliedRel(_, _) => ("tanru".to_string(), "tanru".to_string(), Some("BRIVLA".to_string())),
        JboRel::TanruConnective(_, _, _) => ("tanru-connective".to_string(), "tanru".to_string(), Some("JOI".to_string())),
        JboRel::PermutedRel(_, inner) => convert_rel(inner),
        JboRel::ScalarNegatedRel(_, inner) => {
            let (name, rel_type, selmaho) = convert_rel(inner);
            (format!("not {name}"), rel_type, selmaho)
        }
        JboRel::RVar(n) => (format!("R_{n}"), "var".to_string(), Some("KOhA".to_string())),
        JboRel::BoundRVar(n) => (format!("R_{n}"), "bound-var".to_string(), Some("KOhA".to_string())),
        JboRel::RAss(n) => (format!("R_{n}"), "assigned-var".to_string(), Some("KOhA".to_string())),
        JboRel::UnboundBribasti(_) => ("unbound".to_string(), "unbound".to_string(), None),
        JboRel::Moi(_, _) => ("moi".to_string(), "moi".to_string(), Some("MOI".to_string())),
        JboRel::OperatorRel(_) => ("operator".to_string(), "operator".to_string(), None),
        JboRel::VPredRel(_) => ("vpred".to_string(), "vpred".to_string(), None),
        JboRel::AbsPred(_, _) => ("abstraction".to_string(), "abstraction".to_string(), Some("NU".to_string())),
        JboRel::AbsProp(_, _) => ("abstraction-prop".to_string(), "abstraction".to_string(), Some("NU".to_string())),
        JboRel::TagRel(tag) => ("tag".to_string(), "tag".to_string(), get_selmaho_from_tag(tag)),
        JboRel::ModalRel(_, inner) => convert_rel(inner),
    }
}

// Ported from: JboTree.hs :: getSelmahoFromTag
fn get_selmaho_from_tag(tag: &JboTag) -> Option<String> {
    match tag {
        JboTag::DecoratedTagUnits(units) => units.iter().find_map(get_selmaho_from_dtu),
        JboTag::ConnectedTag(_, t1, t2) => get_selmaho_from_tag(t1).or_else(|| get_selmaho_from_tag(t2)),
    }
}

// Ported from: JboTree.hs :: getSelmahoFromDTU
fn get_selmaho_from_dtu(dtu: &DecoratedTagUnit) -> Option<String> {
    get_selmaho_from_tag_unit(&dtu.tag_unit)
}

// Ported from: JboTree.hs :: getSelmahoFromTagUnit
fn get_selmaho_from_tag_unit(unit: &JboTagUnit) -> Option<String> {
    match unit {
        JboTagUnit::TenseCmavo(c) => lookup_selmaho(c),
        JboTagUnit::CAhA(_) => Some("CAhA".to_string()),
        JboTagUnit::FAhA(_, _) => Some("FAhA".to_string()),
        JboTagUnit::ROI(_, _, _) => Some("ROI".to_string()),
        JboTagUnit::TAhE_ZAhO(_, c) => lookup_selmaho(c),
        JboTagUnit::BAI(_) => Some("BAI".to_string()),
        JboTagUnit::FIhO(_) => Some("FIhO".to_string()),
        JboTagUnit::CUhE(_) => Some("CUhE".to_string()),
        JboTagUnit::KI => Some("KI".to_string()),
    }
}

// Ported from: JboTree.hs :: lookupSelmaho
fn lookup_selmaho(cmavo: &str) -> Option<String> {
    let selmaho = if ["pu", "ca", "ba"].contains(&cmavo) {
        "PU"
    } else if ["zi", "za", "zu"].contains(&cmavo) {
        "ZI"
    } else if ["ze'i", "ze'a", "ze'u"].contains(&cmavo) {
        "ZEhA"
    } else if ["vi", "va", "vu"].contains(&cmavo) {
        "VA"
    } else if ["ve'i", "ve'a", "ve'u"].contains(&cmavo) {
        "VEhA"
    } else if ["vi'i", "vi'a", "vi'u"].contains(&cmavo) {
        "VIhA"
    } else if ["fa", "fe", "fi", "fo", "fu"].contains(&cmavo) {
        "FA"
    } else if ["co'i", "co'a", "co'u", "mo'u", "za'o", "de'a", "di'a"].contains(&cmavo) {
        "ZAhO"
    } else {
        return None;
    };
    Some(selmaho.to_string())
}

// Ported from: JboTree.hs :: convertTermToGraph
fn convert_term_to_graph(state: &mut GraphState, term: &JboTerm) -> String {
    let default_key = format!("term:{}", content_hash(&format!("{:?}", term)));
    match term {
        JboTerm::JoikedTerms(joik, t1, t2) => {
            let node_id = get_or_create_node(
                state,
                &default_key,
                "term",
                NodeData::Term { value: format!("joiked:{joik}"), term_type: "joiked".to_string(), selmaho: Some("JOI".to_string()) },
            );
            let t1_id = convert_term_to_graph(state, t1);
            let t2_id = convert_term_to_graph(state, t2);
            add_edge(state, node_id.clone(), t1_id, "t1".to_string());
            add_edge(state, node_id.clone(), t2_id, "t2".to_string());
            node_id
        }
        JboTerm::QualifiedTerm(_, term) => {
            let node_id = get_or_create_node(
                state,
                &default_key,
                "term",
                NodeData::Term { value: "qualified".to_string(), term_type: "qualified".to_string(), selmaho: Some("LAhE".to_string()) },
            );
            let term_id = convert_term_to_graph(state, term);
            add_edge(state, node_id.clone(), term_id, "term".to_string());
            node_id
        }
        JboTerm::Constant(_, args) => {
            let (value, term_type, _, selmaho) = convert_term(term);
            let node_id = get_or_create_node(state, &default_key, "term", NodeData::Term { value, term_type, selmaho });
            add_term_edges(state, &node_id, args, "arg");
            node_id
        }
        JboTerm::JboQuote(_) => {
            let key = format!("term:quote:{}", content_hash(&format!("{:?}", term)));
            let (value, term_type, _, selmaho) = convert_term(term);
            get_or_create_node(state, &key, "term", NodeData::Term { value, term_type, selmaho })
        }
        JboTerm::JboNonJboQuote(s) => {
            let key = format!("term:quote:{}", content_hash(s));
            let (value, term_type, _, selmaho) = convert_term(term);
            get_or_create_node(state, &key, "term", NodeData::Term { value, term_type, selmaho })
        }
        JboTerm::TermWithSides(term, sides) => {
            let term_id = convert_term_to_graph(state, term);
            for side in sides {
                if let Texticule::TexticuleSide(side_type, texticule) = side {
                    let side_id = side_texticule_to_node(state, side_type, texticule);
                    add_edge(state, term_id.clone(), side_id, "side".to_string());
                }
            }
            term_id
        }
        _ => {
            let (value, term_type, key, selmaho) = convert_term(term);
            get_or_create_node(state, &key, "term", NodeData::Term { value, term_type, selmaho })
        }
    }
}

// Ported from: JboTree.hs :: convertTerm
fn convert_term(term: &JboTerm) -> (String, String, String, Option<String>) {
    match term {
        JboTerm::BoundVar(n) => {
            let value = format!("x_{n}");
            (value.clone(), "var".to_string(), format!("term:var:{value}"), Some("KOhA".to_string()))
        }
        JboTerm::Var(n) => {
            let value = format!("v_{n}");
            (value.clone(), "var".to_string(), format!("term:var:{value}"), Some("KOhA".to_string()))
        }
        JboTerm::Constant(n, _) => {
            let value = format!("c{n}");
            (value.clone(), "constant".to_string(), format!("term:constant:{value}"), Some("KOhA".to_string()))
        }
        JboTerm::Named(s) | JboTerm::NonAnaph(s) => (s.clone(), "named".to_string(), format!("term:named:{s}"), Some("KOhA".to_string())),
        JboTerm::PredNamed(_) => ("pred-named".to_string(), "complex".to_string(), "term:complex:pred-named".to_string(), None),
        JboTerm::UnboundSumbasti(_) => ("unbound-sumbasti".to_string(), "complex".to_string(), "term:complex:unbound-sumbasti".to_string(), None),
        JboTerm::JboQuote(_) => ("quote".to_string(), "quote".to_string(), "term:quote:quote".to_string(), Some("LU".to_string())),
        JboTerm::JboErrorQuote(_) => ("error-quote".to_string(), "quote".to_string(), "term:quote:error-quote".to_string(), Some("LU".to_string())),
        JboTerm::JboNonJboQuote(s) => (s.clone(), "quote".to_string(), format!("term:quote:{s}"), Some("ZO".to_string())),
        JboTerm::TheMex(_) => ("mex".to_string(), "complex".to_string(), "term:complex:mex".to_string(), None),
        JboTerm::Value(_) => ("value".to_string(), "value".to_string(), "term:value:value".to_string(), Some("LI".to_string())),
        JboTerm::Valsi(s) => (s.clone(), "value".to_string(), format!("term:value:{s}"), Some("BRIVLA".to_string())),
        JboTerm::Unfilled => ("unfilled".to_string(), "unfilled".to_string(), "term:unfilled".to_string(), None),
        JboTerm::JoikedTerms(joik, _, _) => (format!("joiked:{joik}"), "joiked".to_string(), format!("term:joiked:{joik}"), Some("JOI".to_string())),
        JboTerm::QualifiedTerm(_, _) => ("qualified".to_string(), "qualified".to_string(), "term:qualified".to_string(), Some("LAhE".to_string())),
        JboTerm::TermWithSides(term, _) => convert_term(term),
    }
}

fn side_texticule_to_node(state: &mut GraphState, side_type: &crate::jbo_prop::SideType, texticule: &Texticule) -> String {
    let side_type = format!("{:?}", side_type);
    let content = match texticule {
        Texticule::TexticuleProp(prop) => {
            let prop_id = convert_prop_to_graph(state, prop, None);
            format!("prop:{prop_id}")
        }
        Texticule::TexticuleSide(_, _) => "side".to_string(),
        Texticule::TexticuleFrag(frag) => format!("{:?}", frag),
    };
    get_or_create_node(
        state,
        &format!("side:{side_type}:{content}"),
        "side-texticule",
        NodeData::SideTexticule { side_type, content },
    )
}

fn convert_quantifier(q: &JboQuantifier) -> String {
    match q {
        JboQuantifier::MexQuantifier(mex) => format!("{:?}", mex),
        JboQuantifier::LojQuantifier(q) => q.to_string(),
        JboQuantifier::QuestionQuantifier => "?".to_string(),
        JboQuantifier::RelQuantifier(q) => format!("R({})", convert_quantifier(q)),
    }
}

// Ported from: JboTree.hs :: jboPropToGraph
pub fn jbo_prop_to_graph(prop: &JboProp) -> GraphOutput {
    let mut state = GraphState::new();
    convert_prop_to_graph(&mut state, prop, None);
    GraphOutput { format: "graph".to_string(), nodes: state.nodes, edges: state.edges }
}

// Ported from: JboTree.hs :: jboPropsToGraph
pub fn jbo_props_to_graph(props: &[JboProp]) -> GraphOutput {
    let mut state = GraphState::new();
    for prop in props {
        convert_prop_to_graph(&mut state, prop, None);
    }
    GraphOutput { format: "graph".to_string(), nodes: state.nodes, edges: state.edges }
}
