// Tree Visualization module using Cytoscape.js

let currentLayout = 'dagreH';

// Layout configs derived from legacy parser
const layoutConfigs = {
  dagreH: {
    name: "dagre",
    rankDir: "LR",
    spacingFactor: 1.2,
    padding: 30,
    animate: true,
    animationDuration: 500,
    fit: true
  },
  dagreV: {
    name: "dagre",
    rankDir: "TB",
    spacingFactor: 1.2,
    padding: 30,
    animate: true,
    animationDuration: 500,
    fit: true
  },
  klayH: {
    name: "klay",
    padding: 4,
    nodeDimensionsIncludeLabels: true,
    klay: {
      direction: "RIGHT",
      spacing: 40,
      mergeEdges: false,
      nodeLayering: "NETWORK_SIMPLEX",
    },
    animate: true,
    animationDuration: 500,
    fit: true
  },
  klayV: {
    name: "klay",
    padding: 4,
    nodeDimensionsIncludeLabels: true,
    klay: {
      direction: "DOWN",
      spacing: 40,
      mergeEdges: false,
      nodeLayering: "NETWORK_SIMPLEX",
      nodePlacement: "LINEAR_SEGMENTS",
    },
    animate: true,
    animationDuration: 500,
    fit: true
  },
  grid: { name: "grid", animate: true, fit: true },
  concentric: { name: "concentric", animate: true, fit: true },
  breadthfirst: { name: "breadthfirst", circle: false, animate: true, fit: true },
  cise: { name: "cise", animate: true, fit: true },
  cose: { name: "cose", animate: true, fit: true },
  cola: { name: "cola", animate: true, fit: true, maxSimulationTime: 4000 }
};


function initTreeViz() {
    // Check if cytoscape is loaded
    if (typeof cytoscape === 'undefined') {
        console.warn('Cytoscape.js not loaded');
        return;
    }
    // Register extensions if present
    if (typeof cytoscapeDagre !== 'undefined') cytoscape.use(cytoscapeDagre);
    if (typeof cytoscapeKlay !== 'undefined') cytoscape.use(cytoscapeKlay);
    if (typeof cytoscapeCise !== 'undefined') cytoscape.use(cytoscapeCise);
    if (typeof cytoscapeCola !== 'undefined') cytoscape.use(cytoscapeCola);
}

// ... transformTreeToGraph and helpers remain same ...

function renderTree(treeData, containerId) {
    if (!treeData && !cy) return;
    
    // Store data for layout updates
    if (treeData) {
        window.lastTreeData = treeData;
        window.graphContainerId = containerId;
    } else {
        treeData = window.lastTreeData;
        containerId = window.graphContainerId;
    }

    if (!treeData) return;

    const elements = transformTreeToGraph(treeData);
    const container = document.getElementById(containerId);
    
    if (!container) return;

    // Destroy existing instance if completely new data, otherwise just layout?
    // For simplicity, re-create on renderTree, but updateLayout will just run layout
    if (cy) {
        cy.destroy();
    }

    // Get selected layout from UI if possible, else default
    const selector = document.getElementById('layout-selector');
    if (selector) {
        currentLayout = selector.value;
    }
    
    const layoutConfig = layoutConfigs[currentLayout] || layoutConfigs.dagreH;

    cy = cytoscape({
        container: container,
        elements: elements,
        boxSelectionEnabled: false,
        autounselectify: true,
        style: [
            // ... styles (same as before) ...
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
        ],
        layout: layoutConfig,
        minZoom: 0.1,
        maxZoom: 3,
        wheelSensitivity: 0.2
    });

    // Animate edges
    cy.edges().forEach(loopAnimation);

    // Initial fit
    cy.ready(() => {
        cy.fit(layoutConfig.padding || 30); 
        cy.center();
    });

    // Fit on resize
    window.addEventListener('resize', () => {
        if (cy) {
            cy.fit(layoutConfig.padding || 30);
            cy.center();
        }
    });
}

function updateLayout() {
    if (!cy) return;
    const selector = document.getElementById('layout-selector');
    if (selector) {
        currentLayout = selector.value;
        const layoutConfig = layoutConfigs[currentLayout];
        if (layoutConfig) {
            cy.layout(layoutConfig).run();
        }
    }
}

// Export functions
if (typeof window !== 'undefined') {
    window.renderTree = renderTree;
    window.initTreeViz = initTreeViz;
    window.updateLayout = updateLayout;
}
