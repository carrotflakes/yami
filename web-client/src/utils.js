export function intersect(x0, y0, x1, y1, x2, y2, x3, y3) {
  var denominator = (x1 - x0) * (y3 - y2) - (y1 - y0) * (x3 - x2);
  if(denominator === 0)
    return null;
  var numeratorR = (y0 - y2) * (x3 - x2) - (x0 - x2) * (y3 - y2);
  var r = numeratorR / denominator;
  var numeratorS = (y0 - y2) * (x1 - x0) - (x0 - x2) * (y1 - y0);
  var s = numeratorS / denominator;
  if(r < 0 || r > 1 || s < 0 || s > 1)
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
    for (const [_, n] of [...node.edgesFrom, ...node.edgesTo]) {
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
    pos.push([node, x, y]);
  }
  for (const [node, x, y] of pos) {
    node.x += x, node.y += y;
    node.bbox = null;
  }
}

export function genDrag(e, move, end) {
  const isTouchEvent = e.type === 'touchstart';
  const unwrap = isTouchEvent ? e => e.changedTouches[0] : e => e; // FIXME changedTouches[0] ?
  let x = unwrap(e).clientX, y = unwrap(e).clientY, moved = false;
  const mousemove = e => {
    const {clientX, clientY } = unwrap(e);
    move(clientX - x, clientY - y);
    x = clientX;
    y = clientY;
    moved = true;
    e.preventDefault();
    e.stopPropagation();
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
