<template>
  <div class="component">
    <svg class="canvas"
         :width="width" :height="height"
         :viewBox="`${scrollX} ${scrollY} ${width} ${height}`"
         @mousedown="canvasMousedown($event)"
         @touchstart="canvasMousedown($event)">
      <Arrow v-for="(edge, index) in edges" :key="'edge/' + index"
             :edge="edge"
             :click="clickEdge"
             :nodeName="nodeName"/>
      <g v-for="(node, index) in nodes" :key="'node/' + index"
         v-if="node.visible"
         class="node"
         @mousedown="nodeMousedown($event, node)"
         @touchstart="nodeMousedown($event, node)">
        <rect v-if="node.bbox"
              :x="node.bbox.x - 2" :y="node.bbox.y - 2"
              :width="node.bbox.width + 4" :height="node.bbox.height + 4"
              fill="#ddd" rx="3" ry="3"></rect>
        <text :ref="'node/' + index"
              :x="node.x" :y="node.y"
              text-anchor="middle"
              font-size="16"
              dominant-baseline="central">{{nodeName(node)}}</text>
      </g>
    </svg>
    <input id="queryBox" type="text" v-model="text" @keydown.ctrl.enter="run"/>
    <span class="humbergerButton" @click="() => {showMainMenu=!showMainMenu}">&#x1f354;</span>
    <div v-if="showMainMenu"
         id="mainMenu">
      <div @click="spring">
        spring ({{ springEnabled ? 'on' : 'off'}})
      </div>
      <div @click="autoExpand">
        auto expand{{ autoExpanding ? ' running...' : ''}}
      </div>
    </div>
    <div v-if="currentNode"
         id="nodeMenu"
         :style="{left: `${currentNode.x-scrollX}px`, top: `${currentNode.y+14-scrollY}px`}">
      <div @click="expand(currentNode)">
        expand
      </div>
      <div @click="toggleSignifyNode(currentNode)">
        signify
      </div>
      <div @click="showTable(currentNode)">
        table
      </div>
      <div @click="() => { currentNode.visible = false }">
        hide
      </div>
    </div>
    <div v-if="currentEdge"
         id="edgeMenu"
         :style="{left: `${currentEdge[1].x-scrollX}px`, top: `${currentEdge[1].y+14-scrollY}px`}">
      <div @click="expand(currentEdge[0])">
        expand
      </div>
      <div @click="removeEdge(currentEdge)">
        remove
      </div>
    </div>
  </div>
</template>

<script>
import Arrow from './Arrow.vue'
import {spring, genDrag} from '../utils'
import {yami} from '../yami'

export default {
  name: 'NetworkView',
  components: {
    Arrow
  },
  data() {
    return {
      scrollX: 0,
      scrollY: 0,
      width: document.body.clientWidth,
      height: document.body.clientHeight,
      showMainMenu: false,
      text: '',
      nodes: [],
      currentNode: null,
      currentEdge: null,
      signifyNode: null,
      attributeNodes: [], // TODO
      springEnabled: false,
      autoExpanding: false
    }
  },
  computed: {
    edges() {
      const edges = []
      for (const node of this.nodes) {
        if (!node.visible)
          continue
        for (const [label, right] of node.edgesFrom)
          if (right.visible &&
              !edges.find(([b, l, r]) => label === b && node === l && right === r))
            edges.push([label, node, right])
        for (const [label, left] of node.edgesTo)
          if (left.visible &&
              !edges.find(([b, l, r]) => label === b && left === l && node === r))
            edges.push([label, left, node])
      }
      return edges
    }
  },
  methods: {
    async run() {
      const {text} = this
      {
        if (text === 'showAll') {
          for (const node of this.nodes)
            node.visible = true
          this.text = ''
          return
        }
      }
      {
        const match = text.match(/(\S+) - (\S+) -> (\S+)/)
        if (match) {
          const lbr = match.slice(1, 4).map(x=>this.parse(x))
          console.log(lbr)
          if (!lbr.includes(null)) {
            for (let i in lbr)
              if (lbr[i] === '_') lbr[i] = null
            const res = await yami.addEdge(lbr[1], lbr[0], lbr[2])
            if (res) {
              res.forEach(node => this.addNode(node))
              this.text = ''
              return
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
        node.expandedAt = Date.now()
        this.addNode(node)
        this.text = ''
        return
      }
    },
    async expand(node) {
      this.addNode(await yami.fetchAsVertex(node))
      node.expandedAt = Date.now()
    },
    async autoExpand() {
      this.autoExpanding = !this.autoExpanding
      while (this.autoExpanding) {
        const nodes = this.nodes.filter(x => !x.expandedAt)
        for (const node of nodes) {
          if (this.nodes.includes(node)) {
            await this.expand(node)
            await new Promise(resolve => setTimeout(resolve, 500))
          }
        }
        if (!nodes.length)
          break
      }
      this.autoExpanding = false
    },
    addNode(node) {
      if (!this.nodes.includes(node)) {
        this.$set(node, 'x', (Math.random() * (this.width - 50) | 0) + 25 + this.scrollX)
        this.$set(node, 'y', (Math.random() * (this.height - 50) | 0) + 25 + this.scrollY)
        this.$set(node, 'bbox', null)
        this.$set(node, 'visible', true)
        this.nodes.push(node)
      }

      for (const [label, right] of node.edgesFrom)
        !this.nodes.includes(right) && this.addNode(right)
      for (const [label, left] of node.edgesTo)
        !this.nodes.includes(left) && this.addNode(left)
    },
    clickEdge(edge) {
      this.currentNode = null
      if (this.currentEdge === edge)
        this.currentEdge = null
      else
        this.currentEdge = edge
    },
    async removeEdge(edge) {
      const succeeded = await yami.removeEdge(...edge)
      if (succeeded) {
        this.currentEdge = null;
      } else {
        console.error('remove edge failed :(')
      }
    },
    toggleSignifyNode(node) {
      if (this.signifyNode === node)
        this.signifyNode = null
      else
        this.signifyNode = node
    },
    async spring() {
      this.springEnabled = !this.springEnabled
      while (this.springEnabled) {
        spring(this.nodes.filter(x => x.bbox))
        await new Promise(resolve => setTimeout(resolve, 100))
      }
    },
    nodeMousedown(e, node) {
      genDrag(e, (dx, dy) => {
        node.x += dx
        node.y += dy
        node.bbox = null
      }, moved => {
        if (!moved) {
          this.currentEdge = null
          if (this.currentNode === node)
            this.currentNode = null
          else
            this.currentNode = node
        }
      })
    },
    canvasMousedown(e) {
      genDrag(e, (dx, dy) => {
        this.scrollX -= dx
        this.scrollY -= dy
      }, moved => {
        if (!moved) {
          this.currentNode = null
        }
      })
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
    },
    showTable(node) {
      console.group(this.nodeName(node))
      for (const [label, right] of node.edgesFrom)
        console.log(`-> ${this.nodeName(label)} ${this.nodeName(right)}`)
      for (const [label, left] of node.edgesTo)
        console.log(`<- ${this.nodeName(label)} ${this.nodeName(left)}`)
      console.groupEnd()
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
.component {
  position: relative;
}

.canvas {
  position: absolute;
  top: 0;
  left: 0;
}

.node {
  cursor: pointer;
  user-select: none;
}

.humbergerButton {
  position: absolute;
  font-size: 200%;
  cursor: pointer;
  filter: drop-shadow(0 0 5px rgba(0, 0, 0, 0.5));
}

#queryBox {
  width: 240px;
  position: absolute;
  top: 3px;
  left: 50%;
  margin-left: -120px;
  filter: drop-shadow(0 0 3px rgba(0, 0, 0, 0.3));
  padding: 4px;
  box-sizing: border-box;
}

#mainMenu {
  position: absolute;
  top: 50px;
  left: 4px;
}

#mainMenu, #nodeMenu, #edgeMenu {
  position: absolute;
  display: inline-block;
  padding: 3px;
  background: rgba(255, 255, 255, 0.9);
  border: 1px solid #777;
  color: #777;
  text-align: left;
  user-select: none;
  font-size: 125%;
}

#nodeMenu > div, #mainMenu > div, #edgeMenu > div {
  padding: 3px;
  cursor: pointer;
}
#nodeMenu > div:hover, #mainMenu > div:hover, #edgeMenu > div:hover {
  background: #eee;
}
</style>
