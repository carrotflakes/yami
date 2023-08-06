export function intersect(x0, y0, x1, y1, x2, y2, x3, y3) {
  var denominator = (x1 - x0) * (y3 - y2) - (y1 - y0) * (x3 - x2);
  if (denominator === 0)
    return null;
  var numeratorR = (y0 - y2) * (x3 - x2) - (x0 - x2) * (y3 - y2);
  var r = numeratorR / denominator;
  var numeratorS = (y0 - y2) * (x1 - x0) - (x0 - x2) * (y1 - y0);
  var s = numeratorS / denominator;
  if (r < 0 || r > 1 || s < 0 || s > 1 || Number.isNaN(r) || Number.isNaN(s))
    return null;
  return {x: x0 + r * (x1 - x0), y: y0 + r * (y1 - y0)};
}

function distance(n1, n2) {
  return ((n1.x - n2.x) ** 2 + (n1.y - n2.y) ** 2) ** (1/2);
}

export function spring(nodes) {
  const pos = [];
  for (const node of nodes) {
    const x = 0, y = 0;
    const edges = [...node.edgesFrom, ...node.edgesTo].filter(e => nodes.includes(e[1]));
    for (const [_, n] of edges) {
      const d = Math.log(distance(node, n) / 120) * 0.2;
      x += (n.x - node.x) * d;
      y += (n.y - node.y) * d;
    }
    for (const n of nodes) {
      const dist = distance(node, n);
      if (n === node || 200 < dist) continue;
      const d = -200 / Math.max(10, dist) ** 2;
      x += (n.x - node.x) * d;
      y += (n.y - node.y) * d;
    }
    const d = Math.max(1, (x ** 2 + y ** 2) ** (1/2) / 50);
    pos.push([node, x / d, y / d]);
  }

  for (const [node, x, y] of pos) {
    if (0.25 < x ** 2 + y ** 2) {
      node.x += x, node.y += y;
      node.bbox = null;
    }
  }
}

export function genDrag(e, move, end) {
  const isTouchEvent = e.type === 'touchstart';
  const touchIdentifier = isTouchEvent ? e.changedTouches[0].identifier : null;
  let x, y, moved = false;
  if (isTouchEvent)
    x = e.changedTouches[0].clientX, y = e.changedTouches[0].clientY;
  else
    x = e.clientX, y = e.clientY;
  const mousemove = e => {
    e.preventDefault();
    e.stopPropagation();
    let pos = e;
    if (isTouchEvent) {
      pos = [].find.call(e.changedTouches, t => t.identifier === touchIdentifier);
      if (!pos) return;
    }
    move(pos.clientX - x, pos.clientY - y);
    x = pos.clientX;
    y = pos.clientY;
    moved = true;
  };
  const mouseup = e => {
    if (isTouchEvent) {
      window.removeEventListener('touchmove', mousemove);
      window.removeEventListener('touchend', mouseup);
      window.removeEventListener('touchcancel', mouseup);
    } else {
      window.removeEventListener('mousemove', mousemove);
      window.removeEventListener('mouseup', mouseup);
    }
    e.preventDefault();
    e.stopPropagation();
    end(moved);
  };
  if (isTouchEvent) {
    window.addEventListener('touchmove', mousemove, {passive: false});
    window.addEventListener('touchend', mouseup, {passive: false});
    window.addEventListener('touchcancel', mouseup, {passive: false});
  } else {
    window.addEventListener('mousemove', mousemove);
    window.addEventListener('mouseup', mouseup);
  }
  // e.preventDefault(); ?
  e.stopPropagation();
}
