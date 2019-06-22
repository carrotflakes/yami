<template>
  <div class="hello">
    <svg class="canvas"
         :width="width" :height="height"
         :viewBox="`${scrollX} ${scrollY} ${width} ${height}`"
         @mousedown="canvasMousedown($event)">
      <Arrow v-for="(edge, index) in edges" :key="'edge/' + index"
             :edge="edge"
             :expand="expand"
             :nodeName="nodeName"/>
      <g v-for="(node, index) in nodes" :key="'node/' + index"
         class="node"
         @mousedown="nodeMousedown($event, node)">
        <rect v-if="node.bbox"
              :x="node.bbox.x - 2" :y="node.bbox.y - 2"
              :width="node.bbox.width + 4" :height="node.bbox.height + 4"
              fill="#ddd" rx="3" ry="3"></rect>
        <text :ref="'node/' + index"
              :x="node.x" :y="node.y"
              text-anchor="middle"
              dominant-baseline="central">{{nodeName(node)}}</text>
      </g>
    </svg>
    <input type="text" v-model="text" @keydown.ctrl.enter="run" style="position: relative;"/>
    <div v-if="currentNode"
         id="nodeMenu"
         :style="{left: `${currentNode.x-scrollX}px`, top: `${currentNode.y+14-scrollY}px`}">
      <div @click="expand(currentNode)">
        expand
      </div>
      <div @click="toggleSignifyNode(currentNode)">
        signify
      </div>
    </div>
  </div>
</template>

<script>
import Arrow from './Arrow.vue'
import {YamiClient} from 'yami-client'

const yami = new YamiClient({url: 'http://localhost:3000'})

export default {
  name: 'HelloWorld',
  components: {
    Arrow
  },
  data() {
    return {
      scrollX: 0,
      scrollY: 0,
      width: document.body.clientWidth,
      height: document.body.clientHeight,
      text: '',
      nodes: [],
      edges: [],
      currentNode: null,
      signifyNode: null
    }
  },
  methods: {
    async run() {
      const {text} = this
      {
        const match = text.match(/(\S+) - (\S+) -> (\S+)/)
        if (match) {
          const lbr = match.slice(1, 4).map(x=>this.parse(x))
          console.log(lbr)
          if (!~lbr.indexOf(null)) {
            for (let i in lbr)
              if (lbr[i] === '_') lbr[i] = null
            const res = await yami.addEdge(lbr[1], lbr[0], lbr[2])
            if (res) {
              res.forEach(node => this.addNode(node))
              this.text = ''
            }
          }
          console.log('adding edge failed')
          return
        }
      }
      {
        if (~text.indexOf(' ')) {
          return
        }
        const node = await yami.fetchAsVertex(text)
        this.addNode(node)
        this.text = ''
        return
      }
    },
    async expand(node) {
      this.addNode(await yami.fetchAsVertex(node))
    },
    addNode(node) {
      if (!~this.nodes.indexOf(node)) {
        this.$set(node, 'x', (Math.random() * (this.width - 50) | 0) + 25 + this.scrollX)
        this.$set(node, 'y', (Math.random() * (this.height - 50) | 0) + 25 + this.scrollY)
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
    toggleSignifyNode(node) {
      if (this.signifyNode === node)
        this.signifyNode = null
      else
        this.signifyNode = node
    },
    nodeMousedown(e, node) {
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
        if (!move) {
          if (this.currentNode === node)
            this.currentNode = null
          else
            this.currentNode = node
        }
      }
      window.addEventListener('mousemove', mousemove)
      window.addEventListener('mouseup', mouseup)
      e.stopPropagation()
    },
    canvasMousedown(e) {
      let x = e.clientX, y = e.clientY, move = false
      const mousemove = e => {
        this.scrollX -= e.clientX - x
        this.scrollY -= e.clientY - y
        x = e.clientX
        y = e.clientY
        move = true
        e.stopPropagation()
      }
      const mouseup = e => {
        window.removeEventListener('mousemove', mousemove)
        window.removeEventListener('mouseup', mouseup)
        e.stopPropagation()
        if (!move) {
          this.currentNode = null
        }
      }
      window.addEventListener('mousemove', mousemove)
      window.addEventListener('mouseup', mouseup)
      e.stopPropagation()
    },
    nodeName(node) {
      if (this.signifyNode) {
        const edge = node.edgesTo.find((e) => e[0] === this.signifyNode)
        if (edge && edge[1].isString)
          return edge[1].name
        }
      return node.toString()
    },
    parse(string) {
      if (string === '_') {
        return '_';
      } else if (string[0] === ':') {
        return yami.getSymbol(string)
      } else if (string[0] === '"') {
        return yami.getString(JSON.parse(string))
      }
      if (this.signifyNode) {
        const node = yami.getString(string)
        if (node) {
          const edge = node.edgesFrom.find(e => e[0] === this.signifyNode)
          if (edge)
            return edge[1]
        }
      }
      return null
    }
  },
  mounted() {
    window.addEventListener('resize', this.resize = e => {
      this.width = document.body.clientWidth
      this.height = document.body.clientHeight
    })
  },
  updated() {
    let update = false
    let node
    for (const key in this.$refs)
      if (key.startsWith('node/') && !(node = this.nodes[key.substr('node/'.length)]).bbox)
        node.bbox = this.$refs[key][0].getBBox(), update = true
    if (update)
      this.$forceUpdate()
  },
  beforeDestroy() {
    window,removeEventListener('resize', this.resize)
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

#nodeMenu {
  position: absolute;
  display: inline-block;
  padding: 3px;
  background: rgba(255, 255, 255, 0.9);
  border: 1px solid #777;
  color: #777;
  text-align: left;
}

#nodeMenu > div {
  cursor: pointer;
}
#nodeMenu > div:hover {
  background: #eee;
}
</style>
