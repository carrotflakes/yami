<template>
  <div id="app">
    <NetworkView v-show="mode === 0" :width="width" :height="height"/>
    <TableView v-show="mode === 1"/>
  </div>
</template>

<script>
import NetworkView from './components/NetworkView.vue'
import TableView from './components/TableView.vue'

export default {
  name: 'app',
  components: {
    NetworkView,
    TableView
  },
  data() {
    return {
      width: document.body.clientWidth,
      height: document.body.clientHeight,
      mode: 0
    }
  },
  mounted() {
    window.addEventListener('resize', this.resize = e => {
      this.width = document.body.clientWidth
      this.height = document.body.clientHeight
    })
    window.addEventListener('keydown', this.keydown = e => {
      if (!e.shiftKey && !e.ctrlKey && !e.altKey && e.code === 'Tab') {
        this.mode = (this.mode + 1) % 2
        e.preventDefault()
      }
    })
  },
  beforeDestroy() {
    window,removeEventListener('resize', this.resize)
    window,removeEventListener('keydown', this.keydown)
  }
}
</script>

<style>
html, body, #app {
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 0;
}

#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
</style>
