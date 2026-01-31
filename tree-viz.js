// Tree Visualization module using Cytoscape.js
let cy = null;

const layoutConfigs = {
  dagreH: { name: "dagre", rankDir: "LR", spacingFactor: 1.2, padding: 30, animate: true, animationDuration: 500, fit: true },
  dagreV: { name: "dagre", rankDir: "TB", spacingFactor: 1.2, padding: 30, animate: true, animationDuration: 500, fit: true },
  klayH: { name: "klay", padding: 4, nodeDimensionsIncludeLabels: true, klay: { direction: "RIGHT", spacing: 40, mergeEdges: false, nodeLayering: "NETWORK_SIMPLEX" }, animate: true, animationDuration: 500, fit: true },
  klayV: { name: "klay", padding: 4, nodeDimensionsIncludeLabels: true, klay: { direction: "DOWN", spacing: 40, mergeEdges: false, nodeLayering: "NETWORK_SIMPLEX", nodePlacement: "LINEAR_SEGMENTS" }, animate: true, animationDuration: 500, fit: true },
  grid: { name: "grid", animate: true, fit: true },
  concentric: { name: "concentric", animate: true, fit: true },
  breadthfirst: { name: "breadthfirst", circle: false, animate: true, fit: true },
  cise: { name: "cise", animate: true, fit: true },
  cose: { name: "cose", animate: true, fit: true },
  cola: { name: "cola", animate: true, fit: true, maxSimulationTime: 4000 },
  elk: { name: "elk", algorithm: "mrtree", padding: 20, elk: { 'elk.direction': 'RIGHT' } },
  NLPTree: { renderer: "NLPTree", name: "NLPTree", config: { alignBottom: false } },
  NLPTreeB: { renderer: "NLPTree", name: "NLPTree", config: { alignBottom: true } },
  three: { renderer: "three", name: "three", config: { dagMode: null, dagLevelDistance: 50 } },
  threeLR: { renderer: "three", name: "three", config: { dagMode: 'lr', dagLevelDistance: 50 } },
  threeTD: { renderer: "three", name: "three", config: { dagMode: 'td', dagLevelDistance: 50 } }
};

function initTreeViz() {
    // Check if cytoscape is loaded
    if (typeof cytoscape === 'undefined') {
        console.warn('Cytoscape.js not loaded');
        return;
    }
    // Register extensions
    if (typeof cytoscapeDagre !== 'undefined') cytoscape.use(cytoscapeDagre);
    if (typeof cytoscapeKlay !== 'undefined') cytoscape.use(cytoscapeKlay);
    if (typeof cytoscapeCise !== 'undefined') cytoscape.use(cytoscapeCise);
    if (typeof cytoscapeCola !== 'undefined') cytoscape.use(cytoscapeCola);
    if (typeof cytoscapeElk !== 'undefined') cytoscape.use(cytoscapeElk);
}

// Transform PropTree JSON to Cytoscape elements
function transformTreeToGraph(treeData) {
    const nodes = [];
    const edges = [];
    let idCounter = 0;

    function nextId() { return `n${idCounter++}`; }

    function traverse(node, parentId = null, labelPrefix = '') {
        const id = nextId();
        let label = '';
        let type = 'default';
        let classes = [];

        if (!node) return null;

        switch (node.type) {
            case 'relation':
                label = node.relation.name;
                type = 'relation';
                classes.push('relation');
                if (node.relation.type) classes.push(node.relation.type);
                
                if (node.terms && node.terms.length > 0) {
                    node.terms.forEach((term, idx) => {
                        const termId = nextId();
                        let termLabel = term.value || term.type;
                        if (term.type === 'joiked') termLabel = `${term.joik} (${term.term1.value} ${term.term2.value})`;
                        
                        nodes.push({ 
                            data: { id: termId, label: termLabel, type: 'term' },
                            classes: ['term', term.type]
                        });
                        edges.push({ 
                            data: { source: id, target: termId, label: `x${idx + 1}` } 
                        });
                    });
                }
                break;

            case 'modal':
                label = (node.modal.tag || node.modal.type);
                type = 'modal';
                classes.push('modal');
                if (node.child) traverse(node.child, id);
                break;

            case 'quantified':
                label = `${node.quantifier.quantifier} ${node.quantifier.variable}`;
                type = 'quantifier';
                classes.push('quantifier');
                if (node.restriction) traverse(node.restriction, id, 'restr');
                if (node.child) traverse(node.child, id);
                break;

            case 'connected':
            case 'non-log-connected':
                label = node.connective;
                type = 'connective';
                classes.push('connective');
                if (node.left) traverse(node.left, id, 'L');
                if (node.right) traverse(node.right, id, 'R');
                break;

            case 'not':
                label = '¬';
                type = 'logic';
                classes.push('logic');
                if (node.child) traverse(node.child, id);
                break;
                
            default:
                label = node.type || '?';
        }

        nodes.push({ 
            data: { id, label, type }, 
            classes: classes 
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
            nodes.push({ data: { id: rootId, label: 'Text', type: 'root' }, classes: ['root'] });
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
    const label = node.data('label') || '';
    const charWidth = 8;
    const padding = 20;
    const minWidth = 60;
    return Math.max(minWidth, label.length * charWidth + padding);
}

function getHeight(node) {
    return 40;
}

function getPadding(node) {
    return '10px';
}

const loopAnimation = (eles) => {
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
        .then(() => loopAnimation(eles));
};

// Transform PropTree JSON to NLP Tree Node structure
function transformToNLPFormat(node) {
    if (!node) return null;
    
    // Base object
    const nlpNode = {
        rule: '',
        text: '',
        type: 'NODE', // or ROOT or VALUE
        children: []
    };

    if (Array.isArray(node)) {
        nlpNode.type = 'ROOT';
        nlpNode.rule = 'Text';
        nlpNode.children = node.map(n => transformToNLPFormat(n)).filter(n => n);
        return nlpNode;
    }

    // Determine properties
    switch (node.type) {
        case 'relation':
            nlpNode.rule = node.relation.type || 'BRIVLA'; // fallback
            nlpNode.text = node.relation.name;
            // Terms are children
             if (node.terms && node.terms.length > 0) {
                 nlpNode.children = node.terms.map(term => {
                    let label = term.value || term.type;
                    if (term.type === 'joiked') label = `${term.joik} (${term.term1.value} ${term.term2.value})`;
                    return { rule: 'sumti', text: label, type: 'VALUE', children: [] };
                 });
             }
            break;
        case 'modal':
            nlpNode.rule = 'BAI';
            nlpNode.text = node.modal.tag || node.modal.type;
            if (node.child) nlpNode.children.push(transformToNLPFormat(node.child));
            break;
        case 'quantified':
             nlpNode.rule = 'quant';
             nlpNode.text = `${node.quantifier.quantifier} ${node.quantifier.variable}`;
             if (node.restriction) nlpNode.children.push(transformToNLPFormat(node.restriction));
             if (node.child) nlpNode.children.push(transformToNLPFormat(node.child));
             break;
        case 'connected':
        case 'non-log-connected':
            nlpNode.rule = 'JOI';
            nlpNode.text = node.connective;
            if (node.left) nlpNode.children.push(transformToNLPFormat(node.left));
            if (node.right) nlpNode.children.push(transformToNLPFormat(node.right));
            break;
        default:
            nlpNode.rule = node.type;
            nlpNode.text = '?';
    }

    if(nlpNode.children.length === 0 && !nlpNode.text) {
        nlpNode.type = 'VALUE'; // Leaf
        nlpNode.text = nlpNode.rule;
    }

    return nlpNode;
}

// 3D Force Graph instance
let Graph3D = null;
let nlpTreeInstance = null;

function cleanup() {
    // Clean Cytoscape
    if (cy) {
        // Unmount first
        cy.unmount();
        cy.destroy();
        cy = null;
    }
    
    // Clean 3D
    const container = document.getElementById(window.graphContainerId);
    if (!container) return;
    
    // Clear container content (removes canvas, 3d graph, etc)
    // IMPORTANT: layout-selector is NOT in this container, it's in the header.
    // So clearing this is safe.
    container.innerHTML = '';
    
    if (Graph3D) {
        Graph3D._destructor && Graph3D._destructor();
        Graph3D = null;
    }
    if (nlpTreeInstance) {
        nlpTreeInstance = null;
    }
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
    if (selector) currentLayout = selector.value;
    const config = layoutConfigs[currentLayout] || layoutConfigs.dagreH;

    cleanup();

    // RENDER BASED ON TYPE
    if (config.renderer === 'NLPTree') {
        const nlpData = transformToNLPFormat(treeData);
        if (!nlpData) return;
        
        const canvas = document.createElement("canvas");
        // Ensure high resolution
        canvas.style.width = '100%';
        canvas.style.height = '100%';
        container.appendChild(canvas);
        
        nlpTreeInstance = new NLPTree(canvas);
        nlpTreeInstance.setAlignBottom(!!config.config.alignBottom);
        nlpTreeInstance.draw(nlpData);
    
    } else if (config.renderer === 'three') {
        const { nodes, edges } = transformTreeToGraph(treeData);
        const gData = {
            nodes: nodes.map(n => ({ 
                id: n.data.id, 
                rule: n.data.type, 
                text: n.data.label, 
                color: get3DColor(n.data.type) 
            })),
            links: edges.map(e => ({ source: e.data.source, target: e.data.target }))
        };

        Graph3D = ForceGraph3D()(container)
            .width(container.clientWidth)
            .height(container.clientHeight)
            .graphData(gData)
            .backgroundColor('#101020')
            .linkColor(() => 'rgba(255,255,255,0.6)')
            .nodeCanvasObject((node, ctx, globalScale) => {
                const label = node.text;
                const fontSize = 12/globalScale;
                ctx.font = `${fontSize}px Sans-Serif`;
                const textWidth = ctx.measureText(label).width;
                const bckgDimensions = [textWidth, fontSize].map(n => n + fontSize * 0.2); // some padding

                ctx.fillStyle = 'rgba(0, 0, 0, 0.8)';
                ctx.fillRect(node.x - bckgDimensions[0] / 2, node.y - bckgDimensions[1] / 2, ...bckgDimensions);

                ctx.textAlign = 'center';
                ctx.textBaseline = 'middle';
                ctx.fillStyle = node.color;
                ctx.fillText(label, node.x, node.y);
                
                node.__bckgDimensions = bckgDimensions; // to re-use in nodePointerAreaPaint
            })
            .nodePointerAreaPaint((node, color, ctx) => {
                ctx.fillStyle = color;
                const bckgDimensions = node.__bckgDimensions;
                if (bckgDimensions) {
                    ctx.fillRect(node.x - bckgDimensions[0] / 2, node.y - bckgDimensions[1] / 2, ...bckgDimensions);
                }
            });
            
        if (config.config.dagMode) {
            Graph3D.dagMode(config.config.dagMode)
                   .dagLevelDistance(config.config.dagLevelDistance);
        }
        
    } else {
        // Cytoscape
        const elements = transformTreeToGraph(treeData);
        cy = cytoscape({
            container: container,
            elements: elements,
            boxSelectionEnabled: false,
            autounselectify: true,
            style: getCyStyle(), // Use helper for style
            layout: config,
            minZoom: 0.1,
            maxZoom: 3,
            wheelSensitivity: 0.2
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
}

function get3DColor(type) {
    if (type === 'relation') return '#4f46e5';
    if (type === 'term') return '#0ea5e9';
    if (type === 'connective') return '#f59e0b';
    if (type === 'quantifier') return '#10b981';
    return '#64748b';
}

function getCyStyle() {
    return [
            {
                selector: 'node',
                style: {
                    'label': 'data(label)',
                    'text-valign': 'center',
                    'text-halign': 'center',
                    'color': '#000',
                    'font-family': 'Inter, sans-serif',
                    'font-size': '12px',
                    'font-weight': 'bold',
                    'text-wrap': 'wrap',
                    'text-max-width': '80px',
                    'width': getWidth,
                    'height': getHeight,
                    'padding': getPadding,
                    'shape': 'round-rectangle',
                    'background-color': '#ffe',
                    'border-width': '1px',
                    'border-color': '#333637',
                    'text-opacity': 0.8
                }
            },
            {
                selector: 'node.relation',
                style: {
                    'background-color': '#eef2ff', 
                    'border-color': '#4f46e5',
                    'color': '#312e81'
                }
            },
            {
                selector: 'node.term',
                style: {
                    'background-color': '#f0f9ff', 
                    'border-color': '#0ea5e9',
                    'color': '#0c4a6e',
                    'font-size': '11px'
                }
            },
            {
                selector: 'node.connective',
                style: {
                    'background-color': '#fffbeb', 
                    'border-color': '#f59e0b',
                    'color': '#78350f',
                    'shape': 'diamond'
                }
            },
            {
                selector: 'node.quantifier',
                style: {
                    'background-color': '#ecfdf5', 
                    'border-color': '#10b981',
                    'color': '#064e3b',
                    'shape': 'hexagon'
                }
            },
            {
                selector: 'node.modal',
                style: {
                    'background-color': '#fdf2f8', 
                    'border-color': '#ec4899',
                    'color': '#831843',
                    'shape': 'tag'
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
                    'text-background-color': '#fff',
                    'text-background-opacity': 0.8,
                    'text-background-padding': '2px',
                    'text-rotation': 'autorotate',
                    'line-style': 'dashed',
                    'line-dash-pattern': [8, 4]
                }
            }
        ];
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
