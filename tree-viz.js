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
  cise: { name: "cise", animate: true, fit: true },
  cose: { name: "cose", animate: true, fit: true },
  cola: { name: "cola", animate: true, fit: true, maxSimulationTime: 4000 },
  elk: { name: "elk", algorithm: "mrtree", padding: 20, elk: { 'elk.direction': 'RIGHT' } },
  elk_box: { name: "elk", algorithm: "box", padding: 20 },
  elk_force: { name: "elk", algorithm: "force", padding: 20 },
  elk_layered: { name: "elk", algorithm: "layered", padding: 20 },
  elk_stress: { name: "elk", algorithm: "stress", padding: 20 },
  NLPTree: { renderer: "NLPTree", name: "NLPTree", config: { alignBottom: false } },
  NLPTreeB: { renderer: "NLPTree", name: "NLPTree", config: { alignBottom: true } },
  three: { renderer: "three", name: "three", config: { dagMode: null, dagLevelDistance: 70 } },
  threeLR: { renderer: "three", name: "three", config: { dagMode: 'lr', dagLevelDistance: 70 } },
  threeTD: { renderer: "three", name: "three", config: { dagMode: 'td', dagLevelDistance: 70 } }
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
        if (typeof cytoscapeElk !== 'undefined') cytoscape.use(cytoscapeElk);
        
        // Debugging CiSE dependencies
        console.log('CiSE Dependencies Check:', {
            layoutBase: typeof window.layoutBase,
            coseBase: typeof window.coseBase,
            cytoscapeCise: typeof window.cytoscapeCise,
            cise: typeof window.cise,
            'cytoscape-cise': typeof window['cytoscape-cise']
        });

        // CiSE often has different global names depending on CDN
        const cisePlugin = window.cytoscapeCise || window.cise || window['cytoscape-cise'];
        if (cisePlugin) {
            cytoscape.use(cisePlugin);
            console.log('CiSE plugin registered successfully');
        } else {
            console.warn('CiSE plugin not found in globals', {windowCise: !!window.cise, windowCytoscapeCise: !!window.cytoscapeCise});
        }

        // Handle 3D globals for SpriteText
        if (typeof ForceGraph3D !== 'undefined' && ForceGraph3D.THREE) {
            window.THREE = ForceGraph3D.THREE;
        }

        // Register nodeHtmlLabel
        if (typeof window.cytoscapeNodeHtmlLabel !== 'undefined') {
            window.cytoscapeNodeHtmlLabel(cytoscape);
        }
    } catch (e) {
        console.error('Error registering Cytoscape extensions:', e);
    }
}

// Transform PropTree JSON to Cytoscape elements
function transformTreeToGraph(treeData) {
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
    const currentLayout = selector ? selector.value : 'dagreH';
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
            nodes: nodes.map(n => ({ ...n.data })),
            links: edges.map(e => ({ source: e.data.source, target: e.data.target }))
        };

        const extraRenderers = window.CSS2DRenderer ? [new window.CSS2DRenderer()] : [];

        Graph3D = ForceGraph3D({ extraRenderers })(container)
            .width(container.clientWidth)
            .height(container.clientHeight)
            .graphData(gData)
            .backgroundColor('#050510')
            .showNavInfo(false)
            .linkColor(() => 'rgba(255,255,255,0.4)')
            .linkDirectionalParticles(2)
            .linkDirectionalParticleWidth(1)
            .linkDirectionalParticleSpeed(0.006)
            .linkDirectionalArrowLength(3.5)
            .linkDirectionalArrowRelPos(1)
            .d3Force("collision", d3.forceCollide(node => 15))
            .nodeThreeObject(node => {
                const Sprite = window.SpriteText || SpriteText;
                if (!Sprite) return null;
                const sprite = new Sprite(node.text || node.rule);
                sprite.color = node.color;
                sprite.textHeight = 10;
                sprite.backgroundColor = 'rgba(0,0,0,0.8)';
                sprite.padding = [2, 4];
                sprite.borderRadius = 2;
                return sprite;
            })
            .nodeThreeObjectExtend(true);
            
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
                        <div style="font-size: 11px; font-weight: bold; color: rgba(0,0,0,0.7); text-transform: uppercase; margin-bottom: 2px;">${data.rule}</div>
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
