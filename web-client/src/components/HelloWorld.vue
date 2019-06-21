<template>
  <div class="hello">
    <svg class="canvas"
         :width="width" :height="height"
         :view-box="`0 0 ${width} ${height}`">
      <Arrow v-for="(edge, index) in edges" :key="'edge/' + index"
             :edge="edge"
             :expand="expand"/>
      <g v-for="(node, index) in nodes" :key="'node/' + index"
         class="node"
         @mousedown="mousedown($event, node)">
        <rect v-if="node.bbox"
              :x="node.bbox.x - 2" :y="node.bbox.y - 2"
              :width="node.bbox.width + 4" :height="node.bbox.height + 4"
              fill="#ddd" rx="3" ry="3"></rect>
        <text :ref="'node/' + index"
              :x="node.x" :y="node.y"
              text-anchor="middle"
              dominant-baseline="central">{{node.toString()}}</text>
      </g>
    </svg>
    <input type="text" v-model="text" @keydown.ctrl.enter="add" style="position: relative;"/>
  </div>
</template>

<script>
import Arrow from './Arrow.vue'
import {YamiClient} from 'yami-client';

const yami = new YamiClient({url: 'http://localhost:3000'})

export default {
  name: 'HelloWorld',
  components: {
    Arrow
  },
  data() {
    return {
      width: document.body.clientWidth,
      height: document.body.clientHeight,
      text: '',
      nodes: [],
      edges: []
    }
  },
  methods: {
    async add() {
      const {text} = this
      const node = await yami.fetchAsVertex(text)
      this.text = ''
      this.addNode(node)
    },
    async expand(node) {
      this.addNode(await yami.fetchAsVertex(node))
    },
    addNode(node) {
      if (!~this.nodes.indexOf(node)) {
        this.$set(node, 'x', (Math.random() * 500 | 0) + 50)
        this.$set(node, 'y', (Math.random() * 300 | 0) + 50)
        this.$set(node, 'bbox', null)
        this.nodes.push(node)
      }

      for (const [label, right] of node.edgesFrom)
        if (!this.edges.find(([b, l, r]) => label === b && node === l && right === r))
          this.edges.push([label, node, right]), this.addNode(right)
      for (const [label, left] of node.edgesTo)
        if (!this.edges.find(([b, l, r]) => label === b && left === l && node === r))
          this.edges.push([label, left, node]), this.addNode(left)
    },
    mousedown(e, node) {
      let x = e.clientX, y = e.clientY, move = false
      const mousemove = e => {
        node.x += e.clientX - x
        node.y += e.clientY - y
        node.bbox = null
        x = e.clientX
        y = e.clientY
        move = true
        e.stopPropagation()
      }
      const mouseup = e => {
        window.removeEventListener('mousemove', mousemove)
        window.removeEventListener('mouseup', mouseup)
        e.stopPropagation()
        if (!move)
          this.expand(node)
      }
      window.addEventListener('mousemove', mousemove)
      window.addEventListener('mouseup', mouseup)
      e.stopPropagation()
    }
  },
  mounted() {
    window.addEventListener('resize', this.resize = e => {
      this.width = e.width
      this.height = e.height
    });
  },
  updated() {
    let update = false
    let node
    for (const key in this.$refs)
      if (key.startsWith('node/') && !(node = this.nodes[key.substr('node/'.length)]).bbox)
        node.bbox = this.$refs[key][0].getBBox(), update = true
    if (update)
      this.$forceUpdate()
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.canvas {
  position: absolute;
  top: 0;
  left: 0;
}

.node {
  cursor: pointer;
  user-select: none;
}
</style>
