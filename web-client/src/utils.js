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
