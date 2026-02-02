// Tree Visualization module using Cytoscape.js
let cy = null;

const hashCode = (s) => s.split("").reduce((a, b) => (a << 5) - a + b.charCodeAt(0), 0);
const number2ColorHue = (number) => Math.floor(((number * 360) / 7.618) % 360);
const bgString2Int = (number, { s = "90%", l = "80%" }) => `hsl(${number2ColorHue(hashCode(number))},${s},${l})`;

const layoutConfigs = {
  dagreH: { name: "dagre", rankDir: "LR", spacingFactor: 1.2, padding: 30, animate: true, animationDuration: 500, fit: true },
  dagreV: { name: "dagre", rankDir: "TB", spacingFactor: 1.2, padding: 30, animate: true, animationDuration: 500, fit: true },
  klayH: { name: "klay", padding: 4, nodeDimensionsIncludeLabels: true, klay: { direction: "RIGHT", spacing: 40, mergeEdges: false, nodeLayering: "NETWORK_SIMPLEX" }, animate: true, animationDuration: 500, fit: true },
  klayV: { name: "klay", padding: 4, nodeDimensionsIncludeLabels: true, klay: { direction: "DOWN", spacing: 40, mergeEdges: false, nodeLayering: "NETWORK_SIMPLEX", nodePlacement: "LINEAR_SEGMENTS" }, animate: true, animationDuration: 500, fit: true },
  grid: { name: "grid", animate: true, fit: true },
  concentric: { name: "concentric", animate: true, fit: true },
  breadthfirst: { name: "breadthfirst", circle: false, animate: true, fit: true },
  cose: { name: "cose", animate: true, fit: true },
  cola: { name: "cola", animate: true, fit: true, maxSimulationTime: 4000 }
};

function initTreeViz() {
    // Check if cytoscape is loaded
    if (typeof cytoscape === 'undefined') {
        console.warn('Cytoscape.js not loaded');
        return;
    }
    // Register extensions
    try {
        if (typeof cytoscapeDagre !== 'undefined') cytoscape.use(cytoscapeDagre);
        if (typeof cytoscapeKlay !== 'undefined') cytoscape.use(cytoscapeKlay);
        if (typeof cytoscapeCola !== 'undefined') cytoscape.use(cytoscapeCola);

        // Register nodeHtmlLabel
        if (typeof window.cytoscapeNodeHtmlLabel !== 'undefined') {
            window.cytoscapeNodeHtmlLabel(cytoscape);
        }
    } catch (e) {
        console.error('Error registering Cytoscape extensions:', e);
    }
}

// Transform PropTree JSON or GraphOutput to Cytoscape elements
function transformTreeToGraph(data) {    
    // The new unified graph format from Haskell
    if (data && data.format === 'graph') {
        console.log('✓ Detected GRAPH format');
        return transformGraphFormat(data);
    }
    
    // Fallback for array format (old behavior or multi-result)
    if (Array.isArray(data) && data.length > 0) {
        if (data[0].format === 'graph') {
            console.log('✓ Detected ARRAY of GRAPH format - merging');
            // Merge all graphs, deduplicating nodes by content if possible
            const allNodes = [];
            const allEdges = [];
            const nodeIds = new Set();
            
            data.forEach(g => {
                g.nodes.forEach(node => {
                    if (!nodeIds.has(node.id)) {
                        nodeIds.add(node.id);
                        allNodes.push(node);
                    }
                });
                allEdges.push(...g.edges);
            });
            return transformGraphFormat({ format: 'graph', nodes: allNodes, edges: allEdges });
        }
        
        // Legacy tree format as array
        console.log('→ Using legacy tree format (array)');
        return transformLegacyTreeFormat(data);
    }
    
    console.log('→ Using legacy tree format (object)');
    // Legacy tree format
    return transformLegacyTreeFormat(data);
}

// Transform new graph format to Cytoscape elements
function transformGraphFormat(graphData) {
    console.log('=== transformGraphFormat ===');
    console.log('Nodes count:', graphData.nodes?.length);
    console.log('Edges count:', graphData.edges?.length);
    console.log('Sample node:', graphData.nodes?.[0]);
    console.log('Sample edge:', graphData.edges?.[0]);
    
    const nodes = graphData.nodes.map(node => {
        let rule = node.type;
        let text = '';
        let type = node.type;
        
        // Extract display info from node data
        if (node.data) {
            if (node.data.name) text = node.data.name;
            else if (node.data.value) text = node.data.value;
            else if (node.data.connective) text = node.data.connective;
            else if (node.data.quantifier) {
                text = `${node.data.quantifier} ${node.data.variable}`;
                rule = 'QUANT';
            } else if (node.data.modalType) {
                text = node.data.tag || node.data.modalType;
                rule = 'BAI';
            }
            
            if (node.data.relType) rule = node.data.relType;
            else if (node.data.termType) rule = node.data.termType;
            
            // Allow selmaho to override rule if present (requested by user)
            if (node.data.selmaho) rule = node.data.selmaho;
        }
        
        return {
            data: {
                id: node.id,
                rule: rule,
                text: text,
                type: type,
                display: 1,
                collapse: 0,
                color: bgString2Int(rule, { s: "90%", l: "80%" })
            }
        };
    });
    
    const edges = graphData.edges.map(edge => ({
        data: {
            source: edge.source,
            target: edge.target,
            label: edge.label
        }
    }));
    
    console.log('Transformed nodes count:', nodes.length);
    console.log('Transformed edges count:', edges.length);
    console.log('Sample transformed node:', nodes[0]);
    console.log('Sample transformed edge:', edges[0]);
    
    return { nodes, edges };
}

// Legacy tree format transformation
function transformLegacyTreeFormat(treeData) {
    const nodes = [];
    const edges = [];
    let idCounter = 0;

    function nextId() { return `n${idCounter++}`; }

    function traverse(node, parentId = null, labelPrefix = '') {
        const id = nextId();
        let rule = '';
        let text = '';
        let type = 'default';

        if (!node) return null;

        switch (node.type) {
            case 'relation':
                rule = node.relation.type || 'BRIVLA';
                text = node.relation.name;
                type = 'relation';
                
                if (node.terms && node.terms.length > 0) {
                    node.terms.forEach((term, idx) => {
                        const termId = nextId();
                        let termText = term.value || term.type;
                        if (term.type === 'joiked') termText = `${term.joik} (${term.term1.value} ${term.term2.value})`;
                        
                        nodes.push({ 
                            data: { 
                                id: termId, 
                                rule: 'sumti', 
                                text: termText, 
                                type: 'term',
                                display: 1,
                                collapse: 0,
                                color: bgString2Int('sumti', { s: "90%", l: "80%" })
                            },
                        });
                        edges.push({ 
                            data: { source: id, target: termId, label: `x${idx + 1}` } 
                        });
                    });
                }
                break;

            case 'modal':
                rule = 'BAI';
                text = (node.modal.tag || node.modal.type);
                type = 'modal';
                if (node.child) traverse(node.child, id);
                break;

            case 'quantified':
                rule = 'quant';
                text = `${node.quantifier.quantifier} ${node.quantifier.variable}`;
                type = 'quantifier';
                if (node.restriction) traverse(node.restriction, id, 'restr');
                if (node.child) traverse(node.child, id);
                break;

            case 'connected':
            case 'non-log-connected':
                rule = 'JOI';
                text = node.connective;
                type = 'connective';
                if (node.left) traverse(node.left, id, 'L');
                if (node.right) traverse(node.right, id, 'R');
                break;

            case 'not':
                rule = 'NA';
                text = '¬';
                type = 'logic';
                if (node.child) traverse(node.child, id);
                break;
                
            default:
                rule = node.type || '?';
                text = '';
        }

        nodes.push({ 
            data: { 
                id, 
                rule, 
                text, 
                type,
                display: 1,
                collapse: 0,
                color: bgString2Int(rule, { s: "90%", l: "80%" })
            }
        });

        if (parentId) {
            edges.push({ 
                data: { source: parentId, target: id, label: labelPrefix } 
            });
        }
    }

    if (Array.isArray(treeData)) {
        if (treeData.length > 1) {
            const rootId = 'root';
            nodes.push({ data: { id: rootId, rule: 'Text', text: '', type: 'root', display: 1, collapse: 0, color: '#f8fafc' } });
            treeData.forEach(tree => traverse(tree, rootId));
        } else {
            treeData.forEach(tree => traverse(tree));
        }
    } else {
        traverse(treeData);
    }

    return { nodes, edges };
}


function getWidth(node) {
    const ctx = document.createElement("canvas").getContext("2d");
    ctx.font = "bold 12px Inter, sans-serif";
    const ruleWidth = ctx.measureText(node.data('rule') || "").width;
    ctx.font = "italic 11px Inter, sans-serif";
    const textWidth = ctx.measureText(node.data('text') || "").width;
    return Math.max(ruleWidth, textWidth, 60) + 30;
}

function getHeight(node) {
    return (node.data('text') && node.data('rule')) ? 60 : 45;
}

function getPadding(node) {
    return '10px';
}

const loopAnimation = (eles) => {
    if (!cy || cy.destroyed()) return;
    const ani = eles.animation(
        {
            style: {
                "line-dash-offset": 24,
                "line-dash-pattern": [8, 4],
            },
        },
        {
            duration: 1450,
        }
    );

    ani
        .reverse()
        .play()
        .promise("complete")
        .then(() => {
            if (cy && !cy.destroyed()) {
                loopAnimation(eles);
            }
        });
};

function cleanup() {
    // Clean Cytoscape
    if (cy) {
        // Unmount first
        cy.unmount();
        cy.destroy();
        cy = null;
    }
    
    const container = document.getElementById(window.graphContainerId);
    if (!container) return;
    
    // Clear container content (removes canvas, etc)
    container.innerHTML = '';
}

function renderTree(treeData, containerId) {
    if (treeData) {
        window.lastTreeData = treeData;
        window.graphContainerId = containerId;
    } else {
        treeData = window.lastTreeData;
        containerId = window.graphContainerId;
    }

    if (!treeData || !containerId) return;

    const container = document.getElementById(containerId);
    if (!container) return;

    // Get layout
    const selector = document.getElementById('layout-selector');
    const currentLayout = selector ? selector.value : 'dagreH';
    const config = layoutConfigs[currentLayout] || layoutConfigs.dagreH;

    cleanup();

    // Cytoscape
    const elements = transformTreeToGraph(treeData);
    cy = cytoscape({
        container: container,
        elements: elements,
        boxSelectionEnabled: false,
        autounselectify: true,
        style: [
            {
                selector: 'node',
                style: {
                    'label': '', // Labels are handled by nodeHtmlLabel
                    'width': getWidth,
                    'height': getHeight,
                    'shape': 'round-rectangle',
                    'background-color': 'data(color)',
                    'border-width': '1px',
                    'border-color': '#333637',
                    'text-opacity': 0.8
                }
            },
            {
                selector: 'edge',
                style: {
                    'width': 2,
                    'line-color': '#aaa', 
                    'target-arrow-color': '#aaa',
                    'target-arrow-shape': 'triangle',
                    'curve-style': 'bezier',
                    'label': 'data(label)',
                    'font-size': '10px',
                    'color': '#555',
                    'text-background-color': '#f8fafc',
                    'text-background-opacity': 0.8,
                    'text-background-padding': '2px',
                    'text-rotation': 'autorotate',
                    'line-style': 'dashed',
                    'line-dash-pattern': [8, 4]
                }
            }
        ],
        layout: config,
        minZoom: 0.1,
        maxZoom: 3,
        wheelSensitivity: 0.2
    });

    if (typeof cy.nodeHtmlLabel === 'function') {
        cy.nodeHtmlLabel([{
            query: 'node',
            tpl: (data) => `
                <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; width: 100%; font-family: Inter, sans-serif; pointer-events: none;">
                    <div style="font-size: 11px; font-weight: bold; color: rgba(0,0,0,0.7); margin-bottom: 2px;">${data.rule}</div>
                    ${data.text ? `<div style="font-size: 12px; font-style: italic; color: #000; font-weight: 500;">${data.text}</div>` : ''}
                </div>
            `
        }]);
    }

    cy.on('tap', 'node', function() {
        const successors = this.successors();
        if (this.data('collapse')) {
            successors.show();
            this.data('collapse', 0);
        } else {
            successors.hide();
            this.data('collapse', 1);
        }
    });
    
    cy.edges().forEach(loopAnimation);
    cy.ready(() => {
        cy.fit(config.padding || 30); 
        cy.center();
    });
    
    // Force fit after a delay to handle container transition/animations
    setTimeout(() => {
        if (cy) {
            cy.fit(config.padding || 30);
            cy.center();
        }
    }, 500);
}

function updateLayout() {
   if (window.lastTreeData) {
       renderTree(window.lastTreeData, window.graphContainerId);
   }
}

// Export functions
if (typeof window !== 'undefined') {
    window.renderTree = renderTree;
    window.initTreeViz = initTreeViz;
    window.updateLayout = updateLayout;
}
