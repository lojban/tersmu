// Tree Visualization module using Cytoscape.js

let cy = null;

function initTreeViz() {
    // Check if cytoscape is loaded
    if (typeof cytoscape === 'undefined') {
        console.warn('Cytoscape.js not loaded');
        return;
    }
    // Register dagre if present and not already registered
    if (typeof cytoscapeDagre !== 'undefined') {
        cytoscape.use(cytoscapeDagre);
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
        let label = '';
        let type = 'default';
        let classes = [];

        if (!node) return null;

        // Determine node properties based on type
        switch (node.type) {
            case 'relation':
                label = node.relation.name;
                type = 'relation';
                classes.push('relation');
                if (node.relation.type) classes.push(node.relation.type);
                
                // Process terms
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

    // Handle array of trees (e.g. multiple sentences)
    if (Array.isArray(treeData)) {
        // Create a virtual root if multiple sentences
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

function renderTree(treeData, containerId) {
    if (!treeData) return;
    
    const elements = transformTreeToGraph(treeData);
    const container = document.getElementById(containerId);
    
    if (!container) return;

    // Destroy existing instance
    if (cy) {
        cy.destroy();
    }

    cy = cytoscape({
        container: container,
        elements: elements,
        style: [
            {
                selector: 'node',
                style: {
                    'label': 'data(label)',
                    'text-valign': 'center',
                    'text-halign': 'center',
                    'color': '#fff',
                    'font-family': 'Inter, sans-serif',
                    'font-size': '12px',
                    'font-weight': 'bold',
                    'text-wrap': 'wrap',
                    'text-max-width': '80px',
                    'width': 'label',
                    'height': 'label',
                    'padding': '10px',
                    'shape': 'round-rectangle',
                    'background-color': '#64748b' // default slate-500
                }
            },
            {
                selector: 'node.relation',
                style: {
                    'background-color': '#4f46e5', // indigo-600
                    'shape': 'ellipse'
                }
            },
            {
                selector: 'node.term',
                style: {
                    'background-color': '#0ea5e9', // sky-500
                    'shape': 'round-rectangle',
                    'font-size': '11px'
                }
            },
            {
                selector: 'node.term.constant',
                style: { 'background-color': '#6366f1' } // indigo-500
            },
            {
                selector: 'node.term.named',
                style: { 'background-color': '#8b5cf6' } // violet-500
            },
            {
                selector: 'node.connective',
                style: {
                    'background-color': '#f59e0b', // amber-500
                    'shape': 'diamond'
                }
            },
            {
                selector: 'node.quantifier',
                style: {
                    'background-color': '#10b981', // emerald-500
                    'shape': 'hexagon'
                }
            },
            {
                selector: 'node.modal',
                style: {
                    'background-color': '#ec4899', // pink-500
                    'shape': 'tag'
                }
            },
            {
                selector: 'edge',
                style: {
                    'width': 2,
                    'line-color': '#cbd5e1', // slate-300
                    'target-arrow-color': '#cbd5e1',
                    'target-arrow-shape': 'triangle',
                    'curve-style': 'bezier',
                    'label': 'data(label)',
                    'font-size': '10px',
                    'color': '#94a3b8',
                    'text-background-color': '#fff',
                    'text-background-opacity': 1,
                    'text-background-padding': '2px',
                    'text-rotation': 'autorotate'
                }
            }
        ],
        layout: {
            name: 'dagre',
            rankDir: 'TB',
            spacingFactor: 1.2,
            padding: 20,
            animate: true,
            animationDuration: 500
        },
        minZoom: 0.5,
        maxZoom: 3,
        wheelSensitivity: 0.2
    });

    // Fit on resize
    window.addEventListener('resize', () => {
        if (cy) cy.fit();
    });
}

// Export functions
if (typeof window !== 'undefined') {
    window.renderTree = renderTree;
    window.initTreeViz = initTreeViz;
}
