"use strict";

const NLP_NODE_PADDING = 20;

const NLPNodeType = {
  ROOT: "ROOT",
  NODE: "NODE",
  VALUE: "VALUE",
};

const MAX_ZOOM = 5;
const MIN_ZOOM = 0.1;
const SCROLL_SENSITIVITY = 0.0005;

class NLPTree {
  constructor(canvas) {
    this.canvas = canvas;
    this.init();
  }

  init() {
    this.syntaxTree = null;
    this.nodeColor = true;
    this.font = "sans-serif";
    this.terminalColor = "#BB1100";
    this.labelColor = "#0044BB";
    this.ruleColor = "#666666";
    this.fontSize = 16;
    this.triangles = true;
    this.subscript = true;
    this.alignTerminals = false;
    this.vscaler = 1;
    this.context = this.canvas.getContext("2d");
    this.cameraOffset = { x: this.canvas.width / 2, y: this.canvas.height / 2 };
    this.cameraZoom = 1;
    this.isDragging = false;
    this.dragStart = { x: 0, y: 0 };
    this.initialPinchDistance = null;
    this.lastZoom = this.cameraZoom;

    this.addEventListeners();
    // this.resizeCanvas(); // Will be called externally or on first draw
  }

  addEventListeners() {
    this.canvas.addEventListener("mousedown", this.onPointerDown.bind(this));
    this.canvas.addEventListener("touchstart", (e) =>
      this.handleTouch(e, this.onPointerDown.bind(this))
    );
    this.canvas.addEventListener("mouseup", this.onPointerUp.bind(this));
    this.canvas.addEventListener("touchend", (e) =>
      this.handleTouch(e, this.onPointerUp.bind(this))
    );
    this.canvas.addEventListener("mousemove", this.onPointerMove.bind(this));
    this.canvas.addEventListener("touchmove", (e) =>
      this.handleTouch(e, this.onPointerMove.bind(this))
    );
    this.canvas.addEventListener("wheel", (e) => {
        e.preventDefault();
        this.adjustZoom(e.deltaY * SCROLL_SENSITIVITY);
    });
    // window.addEventListener("resize", this.resizeCanvas.bind(this));
  }

  CCtextWidth = (t) => {
    this.context.font = [this.fontSize, "px ", this.font].join("");
    return this.context.measureText(t).width;
  };

  CCtext = ({
    text,
    x,
    y,
    italic,
    bold,
    fontSize = this.fontSize,
    shadowColor = "transparent",
    shadowBlur = 0,
    shadowOffsetX = 0,
    shadowOffsetY = 0,
    fillStyle,
  }) => {
    this.context.font = [
      italic ? "italic" : null,
      bold ? "bold" : null,
      `${fontSize}px ${this.font}`,
    ]
      .filter(Boolean)
      .join(" ");
    if (fillStyle) this.CCsetFillStyle(fillStyle);

    this.CCsetShadow({ shadowColor, shadowBlur, shadowOffsetX, shadowOffsetY });
    this.context.fillText(text, x, y);
    this.CCsetShadow({
      shadowColor: "transparent",
      shadowBlur: 0,
      shadowOffsetX: 0,
      shadowOffsetY: 0,
    });
  };

  CCsetShadow = ({ shadowColor, shadowBlur, shadowOffsetX, shadowOffsetY }) => {
    this.context.shadowColor = shadowColor;
    this.context.shadowBlur = shadowBlur;
    this.context.shadowOffsetX = shadowOffsetX;
    this.context.shadowOffsetY = shadowOffsetY;
  };

  CCsetFillStyle = (s) => {
    this.context.fillStyle = s;
  };

  CCsetStrokeStyle = (s) => {
    this.context.strokeStyle = s;
  };

  CCsetLineWidth = (w) => {
    this.context.lineWidth = w;
  };

  CCline = (x1, y1, x2, y2) => {
    const ctx = this.context;
    ctx.beginPath();
    ctx.moveTo(x1, y1);
    ctx.lineTo(x2, y2);
    ctx.stroke();
  };

  CCtriangle = (x1, y1, x2, y2, x3, y3, fill = false) => {
    const ctx = this.context;
    ctx.beginPath();
    ctx.moveTo(x1, y1);
    ctx.lineTo(x2, y2);
    ctx.lineTo(x3, y3);
    ctx.lineTo(x1, y1);
    if (fill) ctx.fill();
    ctx.stroke();
  };

  CCcurve = (x1, y1, x2, y2, cx1, cy1, cx2, cy2) => {
    const ctx = this.context;
    ctx.beginPath();
    ctx.moveTo(x1, y1);
    ctx.bezierCurveTo(cx1, cy1, cx2, cy2, x2, y2);
    ctx.stroke();
  };

  resizeCanvas = (w, h) => {
      // Use parent container dimensions
      const parent = this.canvas.parentElement;
      if (parent) {
        this.canvas.width = parent.clientWidth;
        this.canvas.height = parent.clientHeight;
      } else {
        this.canvas.width = w || window.innerWidth;
        this.canvas.height = h || 400; 
      }

    this.context.clearRect(0, 0, this.canvas.width, this.canvas.height);
    this.context.textAlign = "center";
    this.context.textBaseline = "top";

    const ctx = this.context;
    // this.context.width = this.canvas.width; // Not standards compliant

    ctx.scale(this.cameraZoom, this.cameraZoom);
    ctx.translate(
      -this.canvas.width / 2 + this.cameraOffset.x,
      -this.canvas.height / 2 + this.cameraOffset.y
    );
  };

  getEventLocation = (e) => {
    if (e.touches && e.touches.length === 1) {
      const rect = this.canvas.getBoundingClientRect();
      return { x: e.touches[0].clientX - rect.left, y: e.touches[0].clientY - rect.top };
    } else if (e.clientX && e.clientY) {
      const rect = this.canvas.getBoundingClientRect();
      return { x: e.clientX - rect.left, y: e.clientY - rect.top };
    }
  };

  onPointerDown = (e) => {
    this.isDragging = true;
    const loc = this.getEventLocation(e);
    if (loc) {
        this.dragStart.x = loc.x / this.cameraZoom - this.cameraOffset.x;
        this.dragStart.y = loc.y / this.cameraZoom - this.cameraOffset.y;
    }
  };

  onPointerUp = () => {
    this.isDragging = false;
    this.initialPinchDistance = null;
    this.lastZoom = this.cameraZoom;
  };

  onPointerMove = (e) => {
    if (this.isDragging) {
      const loc = this.getEventLocation(e);
      if (loc) {
          this.cameraOffset.x = loc.x / this.cameraZoom - this.dragStart.x;
          this.cameraOffset.y = loc.y / this.cameraZoom - this.dragStart.y;
          this.draw(this.syntaxTree);
      }
    }
  };

  handleTouch = (e, singleTouchHandler) => {
    if (e.touches.length === 1) {
      singleTouchHandler(e);
    } else if (e.type === "touchmove" && e.touches.length === 2) {
      this.isDragging = false;
      this.handlePinch(e);
    }
  };

  handlePinch = (e) => {
    e.preventDefault();

    let touch1 = { x: e.touches[0].clientX, y: e.touches[0].clientY };
    let touch2 = { x: e.touches[1].clientX, y: e.touches[1].clientY };

    let currentDistance =
      (touch1.x - touch2.x) ** 2 + (touch1.y - touch2.y) ** 2;

    if (this.initialPinchDistance === null) {
      this.initialPinchDistance = currentDistance;
    } else {
      this.adjustZoom(null, currentDistance / this.initialPinchDistance);
    }
  };

  adjustZoom = (zoomAmount, zoomFactor) => {
      if (zoomAmount) {
        this.cameraZoom -= zoomAmount;
      } else if (zoomFactor) {
        // this.cameraZoom = zoomFactor * this.lastZoom; // slightly buggy in original?
        this.cameraZoom = zoomFactor * this.lastZoom;
      }

      this.cameraZoom = Math.min(this.cameraZoom, MAX_ZOOM);
      this.cameraZoom = Math.max(this.cameraZoom, MIN_ZOOM);

      this.draw(this.syntaxTree);
  };

  drawRect = (ctx, x, y, width, height) => {
    ctx.fillRect(x, y, width, height);
  };

  draw = (syntaxTree) => {
    if(!syntaxTree) return;
    this.syntaxTree = syntaxTree;
    
    // Clear canvas with identity matrix for clearing
    this.context.save();
    this.context.setTransform(1, 0, 0, 1, 0, 0);
    this.context.clearRect(0, 0, this.canvas.width, this.canvas.height);
    this.context.restore();

    this.resizeCanvas(); // re-apply transform

    const drawables = drawableFromNode(this, this.syntaxTree);
    const maxDepth = getMaxDepth(drawables);

    if (this.alignTerminals) moveLeafsToBottom(drawables, maxDepth);
    if (this.subscript) calculateAutoSubscript(drawables);

    const hasArrow = calculateDrawablePositions(this, drawables, this.vscaler);
    const arrowSet = makeArrowSet(drawables, this.fontSize);
    
    // Auto-centering logic usually here, but original used resizeCanvas... 
    // We already centered with cameraOffset.
    
    drawables.children.forEach((child) => this.drawNode(child));
    this.drawArrows(arrowSet.arrows);
  };

  drawNode = (drawable) => {
    this.drawLabel(drawable);

    drawable.children.forEach((child) => {
      this.drawNode(child);
      this.drawConnector(drawable, child);
    });
  };

  drawLabel = (drawable) => {
    const terminalCoincidesWithRule = doesTerminalCoincideWithRule(drawable);

    this.CCtext({
      fillStyle: terminalCoincidesWithRule
        ? this.terminalColor
        : this.ruleColor,
      text: drawable.rule.replace(/_/g, "-"),
      x: getDrawableCenter(drawable),
      y: terminalCoincidesWithRule ? drawable.top + 7 : drawable.top - 7,
      italic: true,
    });

    if (!terminalCoincidesWithRule) {
      this.CCtext({
        fillStyle: drawable.isLeaf ? this.terminalColor : this.labelColor,
        text: drawable.text,
        x: getDrawableCenter(drawable),
        y: drawable.top + 7,
        shadowColor: "#666",
        shadowBlur: 4,
        shadowOffsetX: 0,
        shadowOffsetY: 0,
      });
    }
  };

  drawSubscript = (drawable) => {
    if (drawable.subscript == null || drawable.subscript === "") return;
    let offset =
      1 + getDrawableCenter(drawable) + this.CCtextWidth(drawable.rule) / 2;
    offset += this.CCtextWidth(drawable.subscript) / 2;
    this.CCtext({
      fillStyle: "#666",
      text: drawable.subscript,
      x: offset,
      y: drawable.top + this.fontSize / 2 - 9,
      italic: true,
      fontSize: this.fontSize * 0.75,
    });
  };

  drawConnector = (parent, child) => {
    const terminalCoincidesWithRule = doesTerminalCoincideWithRule(child);

    if (this.triangles && child.isLeaf && child.rule && child.rule.includes(" ")) {
      const textWidth = this.CCtextWidth(child.rule);
      this.CCtriangle(
        getDrawableCenter(parent),
        parent.top + this.fontSize + 2,
        getDrawableCenter(child) + textWidth / 2 - 4,
        child.top - 3,
        getDrawableCenter(child) - textWidth / 2 + 4,
        child.top - 3
      );
    } else {
      this.CCline(
        getDrawableCenter(parent),
        parent.top + this.fontSize + 9,
        getDrawableCenter(child),
        terminalCoincidesWithRule ? child.top + 3 : child.top - 10
      );
    }
  };

  drawArrows = (arrows) => {
    const arrowColor = this.nodeColor ? "#909" : "#999";
    this.CCsetFillStyle(arrowColor);
    this.CCsetStrokeStyle("white");
    this.CCsetLineWidth(2);

    for (const arrow of arrows) {
      this.CCcurve(
        arrow.fromX,
        arrow.fromY,
        arrow.toX,
        arrow.toY,
        arrow.fromX,
        arrow.bottom,
        arrow.toX,
        arrow.bottom
      );
      if (arrow.endsTo) this.drawArrowHead(arrow.toX, arrow.toY);
      if (arrow.endsFrom) this.drawArrowHead(arrow.fromX, arrow.fromY);
    }
  };

  drawArrowHead = (x, y) => {
    const cx = this.fontSize / 4;
    const cy = this.fontSize / 2;
    this.CCtriangle(x, y, x - cx, y + cy, x + cx, y + cy, true);
  };

  setAlignBottom = (a) => {
    this.alignTerminals = a;
  };
}

class Arrow {
  constructor(fromX, fromY, toX, toY, bottom, endsTo, endsFrom) {
    this.fromX = fromX;
    this.fromY = fromY;
    this.toX = toX;
    this.toY = toY;
    this.bottom = bottom;
    this.endsTo = endsTo;
    this.endsFrom = endsFrom;
  }
}

class ArrowSet {
  constructor() {
    this.arrows = [];
    this.maxBottom = 0;
  }

  add(arrow) {
    this.arrows.push(arrow);
    this.maxBottom = Math.max(this.maxBottom, arrow.bottom);
  }

  concatenate(arrowSet) {
    this.arrows.push(...arrowSet.arrows);
    this.maxBottom = Math.max(this.maxBottom, arrowSet.maxBottom);
  }
}

const doesTerminalCoincideWithRule = (drawable) =>
  drawable.isLeaf && drawable.rule === drawable.text;

const drawableFromNode = (tree, node, depth = -1) => {
  const drawable = {
    rule: node.rule,
    text: node.text,
    subscript: node.subscript,
    width: getNodeWidth(tree, node),
    depth: depth,
    isLeaf: node.type == NLPNodeType.VALUE,
    arrow: "arrow" in node ? node.arrow : null,
    children: [],
    // Defaults for position
    top: 0,
    left: 0
  };

  if (node.type !== NLPNodeType.VALUE) {
    node.children.forEach((child) => {
      drawable.children.push(drawableFromNode(tree, child, depth + 1));
    });
  }

  return drawable;
};

const getNodeWidth = (tree, node) => {
  let labelWidth =
    node.type !== NLPNodeType.ROOT
      ? Math.max(tree.CCtextWidth(node.rule || ""), tree.CCtextWidth(node.text || "")) +
        NLP_NODE_PADDING
      : 0;

  if (node.subscript)
    labelWidth += ((tree.CCtextWidth(node.subscript) * 3) / 4) * 2;

  if (node.type !== NLPNodeType.VALUE) {
    return Math.max(labelWidth, getChildWidth(tree, node));
  } else {
    return labelWidth;
  }
};

const calculateDrawablePositions = (
  tree,
  drawable,
  vscaler,
  parentOffset = 0
) => {
  let offset = 0;
  let scale = 1;
  let hasArrow = drawable.arrow;

  if (drawable.depth >= 0) {
    const childWidth = getDrawableChildWidth(drawable);
    if (drawable.width > childWidth) scale = drawable.width / childWidth;
  }

  drawable.children.forEach((child) => {
    child.top = child.depth * (tree.fontSize * 3 * vscaler) + NLP_NODE_PADDING / 2;
    child.left = offset + parentOffset;
    child.width *= scale;
    const childHasArrow = calculateDrawablePositions(
      tree,
      child,
      vscaler,
      child.left
    );
    if (childHasArrow) hasArrow = true;
    offset += child.width;
  });

  return hasArrow;
};

const getChildWidth = (tree, node) => {
  if (node.type === NLPNodeType.VALUE) return 0;
  let childWidth = 0;
  node.children.forEach((child) => {
    childWidth += getNodeWidth(tree, child);
  });
  return childWidth;
};

const getDrawableChildWidth = (drawable) => {
  if (drawable.children.length === 0) return drawable.width;
  let childWidth = 0;
  drawable.children.forEach((child) => {
    childWidth += child.width;
  });
  return childWidth;
};

const getMaxDepth = (drawable) => {
  let maxDepth = drawable.depth;
  drawable.children.forEach((child) => {
    const childDepth = getMaxDepth(child);
    maxDepth = Math.max(maxDepth, childDepth);
  });
  return maxDepth;
};

const moveLeafsToBottom = (drawable, bottom) => {
  if (drawable.isLeaf) {
    drawable.depth = bottom;
  }
  drawable.children.forEach((child) => moveLeafsToBottom(child, bottom));
};

const calculateAutoSubscript = (drawables) => {
  const countMap = countNodes(drawables);
  Array.from(countMap.keys()).forEach((key) => {
    if (countMap.get(key) === 1) {
      countMap.delete(key);
    }
  });
  assignSubscripts(drawables, Array.from(countMap.keys()), new Map());
};

const assignSubscripts = (drawable, keys, tally) => {
  if (
    !drawable.isLeaf &&
    (drawable.subscript == null || drawable.subscript === "") &&
    keys.includes(drawable.rule)
  ) {
    mapInc(tally, drawable.rule);
    drawable.subscript = String(tally.get(drawable.rule));
  }
  drawable.children.forEach((child) => assignSubscripts(child, keys, tally));
};

const countNodes = (drawable) => {
  let countMap = new Map();
  if (drawable.isLeaf) {
    return countMap;
  }
  if (drawable.subscript == null || drawable.subscript === "") {
    mapInc(countMap, drawable.rule);
  }
  drawable.children.forEach((child) => {
    const childMap = countNodes(child);
    countMap = new Map([...countMap, ...childMap]);
  });
  return countMap;
};

const findTarget = (drawable, arrowIdx) => {
  const [, target] = findTargetLeaf(drawable, arrowIdx, 0);
  return target;
};

const findTargetLeaf = (drawable, arrowIdx, count) => {
  if (drawable.isLeaf && ++count === arrowIdx) {
    return [count, drawable];
  }
  for (const child of drawable.children) {
    let target = null;
    [count, target] = findTargetLeaf(child, arrowIdx, count);
    if (target !== null) {
      return [count, target];
    }
  }
  return [count, null];
};

const mapInc = (map, key) => {
  if (!map.has(key)) {
    map.set(key, 1);
  } else {
    map.set(key, map.get(key) + 1);
  }
};

const getDrawableCenter = (drawable) => drawable.left + drawable.width / 2;

const findMaxDepthBetween = (drawable, left, right, maxY = 0) => {
  drawable.children.forEach((child) => {
    maxY = Math.max(findMaxDepthBetween(child, left, right, maxY), maxY);
  });

  if (drawable.isLeaf && drawable.left >= left && drawable.left <= right) {
    maxY = Math.max(drawable.top, maxY);
  }

  return maxY;
};

const makeArrowSet = (root, fontSize) => makeArrowSetOn(root, root, fontSize);

const makeArrowSetOn = (root, drawable, fontSize) => {
  const arrowSet = new ArrowSet();

  drawable.children.forEach((child) => {
    arrowSet.concatenate(makeArrowSetOn(root, child, fontSize));
  });

  if (!drawable.isLeaf || !drawable.arrow) return arrowSet;

  const target = findTarget(root, drawable.arrow.target);
  if (!target) return arrowSet;

  const from = {
    x: getDrawableCenter(drawable),
    y: drawable.top + fontSize * 1.2,
  };

  const to = {
    x: getDrawableCenter(target),
    y: target.top + fontSize * 1.2,
  };

  const bottom =
    1.4 *
    findMaxDepthBetween(
      root,
      Math.min(drawable.left, target.left),
      Math.max(drawable.left, target.left)
    );

  const { endsTo, endsFrom } = drawable.arrow.ends;

  arrowSet.add(new Arrow(from.x, from.y, to.x, to.y, bottom, endsTo, endsFrom));

  return arrowSet;
};

// Expose globally
if (typeof window !== 'undefined') {
    window.NLPTree = NLPTree;
    window.NLPNodeType = NLPNodeType;
}
