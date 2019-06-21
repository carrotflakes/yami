<template>
  <rect>
    <text>
      yo
    </text>
  </rect>
</template>

<script>
export default {
  name: 'Text',
  data() {
    return {
      text: '',
      nodes: [],
      edges: []
    }
  },
  methods: {
    f(x) {
      console.log(1)
      console.log(x)
    },
    async add() {
      const t = document.createElementNS('http://www.w3.org/2000/svg', 'text')
      t.textContent = 'aaa'
      console.log(t.getBBox())
      const {text} = this
      const node = await yami.fetchAsVertex(text)
      this.text = ''
      this.addNode(node)
    },
    addNode(node) {
      if (!~this.nodes.indexOf(node)) {
        node.x = (Math.random() * 500 | 0) + 50
        node.y = (Math.random() * 300 | 0) + 50
        this.nodes.push(node);console.log(node)

        for (const [label, right] of node.edgesFrom)
          if (!this.edges.find(([b, l, r]) => label === b && node === l && right === r))
            this.edges.push([label, node, right]), this.addNode(right)
        for (const [label, left] of node.edgesTo)
          if (!this.edges.find(([b, l, r]) => label === b && left === l && node === r))
            this.edges.push([label, left, node]), this.addNode(left)
      }
    }
  },
  mounted() {
    yami;
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.canvas {
  width: 600px;
  height: 400px;
  position: relative;
  background: #eee;
}

.node {
  display: inline-block;
  position: absolute;
  padding: 2px;
  border: 1px solid #aaa;
  border-radius: 3px;
  cursor: pointer;
}
</style>
