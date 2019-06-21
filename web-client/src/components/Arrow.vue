<template>
  <g>
    <line :x1="x1" :y1="y1" :x2="x2" :y2="y2"
          stroke="#ddd" stroke-width="4" marker-end="url(#arrow)"/>
    <text class="label"
          :x="0" :y="0"
          text-anchor="middle"
          dominant-baseline="central"
          :transform="`translate(${(left.x + right.x) / 2} ${(left.y + right.y) / 2}),rotate(${Math.atan2(left.y-right.y, left.x-right.x) * 180 / Math.PI})`"
          color="#666"
          font-size="10"
          @click="expand(label)">
        {{label.toString()}}
    </text>
    <marker id="arrow" viewBox="-5 -5 10 10" orient="auto">
      <polygon points="-5,-5 5,0 -5,5" fill="#ddd" stroke="none" />
    </marker>
  </g>
</template>

<script>
export default {
  name: 'Array',
  props: ['edge', 'expand'],
  data() {
    const [label, left, right] = this.edge;
    return {
      label,
      left,
      right
    }
  },
  computed: {
    x1() { return (this.left.x * 3 + this.right.x) / 4 },
    y1() { return (this.left.y * 3 + this.right.y) / 4 },
    x2() { return (this.right.x * 3 + this.left.x) / 4 },
    y2() { return (this.right.y * 3 + this.left.y) / 4 }
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.label {
  cursor: pointer;
  user-select: none;
}
</style>
