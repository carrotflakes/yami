<template>
  <g>
    <line v-if="start && end" :x1="start.x" :y1="start.y" :x2="end.x" :y2="end.y"
          stroke="#ddd" stroke-width="8" marker-end="url(#arrow)"/>
    <text class="label"
          :x="0" :y="0"
          text-anchor="middle"
          dominant-baseline="central"
          :transform="`translate(${(left.x + right.x) / 2} ${(left.y + right.y) / 2}),rotate(${Math.atan2(left.y-right.y, left.x-right.x) * 180 / Math.PI + 180})`"
          fill="#666"
          font-size="10"
          @click="expand(label)">
        {{nodeName(label)}}
    </text>
    <marker id="arrow" viewBox="-5 -5 10 10" orient="auto">
      <polygon points="0,-5 5,0 0,5" fill="#ddd" stroke="none" />
    </marker>
  </g>
</template>

<script>
import {intersect} from '../utils'

export default {
  name: 'Array',
  props: ['edge', 'expand', 'nodeName'],
  data() {
    const [label, left, right] = this.edge
    return {
      label,
      left,
      right
    }
  },
  computed: {
    start() { return this.left.bbox &&clipByBBox(this.left, this.right, this.left.bbox, -5) },
    end() { return this.right.bbox &&clipByBBox(this.left, this.right, this.right.bbox, 15) }
  }
}

function clipByBBox(p1, p2, bbox, margin) {
  const {x: x1, y: y1} = p1
  const {x: x2, y: y2} = p2
  let {x, y, width: w, height: h} = bbox
  x -= margin
  y -= margin
  w += margin * 2
  h += margin * 2
  let pos;
  if (pos = intersect(x, y, x+w, y, x1, y1, x2, y2))
    return pos
  if (pos = intersect(x+w, y, x+w, y+h, x1, y1, x2, y2))
    return pos
  if (pos = intersect(x+w, y+h, x, y+h, x1, y1, x2, y2))
    return pos
  if (pos = intersect(x, y+h, x, y, x1, y1, x2, y2))
    return pos
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.label {
  cursor: pointer;
  user-select: none;
}
</style>
