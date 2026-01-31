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

// Helper functions from old parser
function getWidth(node) {
    const label = node.data('label') || '';
    // Approximate width calculation
    const charWidth = 8;
    const padding = 20;
    const minWidth = 60;
    return Math.max(minWidth, label.length * charWidth + padding);
}

function getHeight(node) {
    return 40; // Standard height
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
        boxSelectionEnabled: false,
        autounselectify: true,
        style: [
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
                    'background-color': '#eef2ff', // Indigo tint
                    'border-color': '#4f46e5',
                    'color': '#312e81'
                }
            },
            {
                selector: 'node.term',
                style: {
                    'background-color': '#f0f9ff', // Sky tint
                    'border-color': '#0ea5e9',
                    'color': '#0c4a6e',
                    'font-size': '11px'
                }
            },
            {
                selector: 'node.connective',
                style: {
                    'background-color': '#fffbeb', // Amber tint
                    'border-color': '#f59e0b',
                    'color': '#78350f',
                    'shape': 'diamond'
                }
            },
            {
                selector: 'node.quantifier',
                style: {
                    'background-color': '#ecfdf5', // Emerald tint
                    'border-color': '#10b981',
                    'color': '#064e3b',
                    'shape': 'hexagon'
                }
            },
            {
                selector: 'node.modal',
                style: {
                    'background-color': '#fdf2f8', // Pink tint
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
        ],
        layout: {
            name: 'dagre',
            rankDir: 'TB',
            spacingFactor: 1.2,
            padding: 30,
            animate: true,
            animationDuration: 500,
            fit: true
        },
        minZoom: 0.1, // Allow zooming out further
        maxZoom: 3,
        wheelSensitivity: 0.2
    });

    // Animate edges
    cy.edges().forEach(loopAnimation);

    // Initial fit
    cy.ready(() => {
        cy.fit(30); // Fit with 30px padding
        cy.center();
    });

    // Fit on resize
    window.addEventListener('resize', () => {
        if (cy) {
            cy.fit(30);
            cy.center();
        }
    });
}

// Export functions
if (typeof window !== 'undefined') {
    window.renderTree = renderTree;
    window.initTreeViz = initTreeViz;
}
