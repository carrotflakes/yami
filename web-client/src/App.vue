<template>
  <div id="app">
    <NetworkView :width="width" :height="height"/>
  </div>
</template>

<script>
import NetworkView from './components/NetworkView.vue'

export default {
  name: 'app',
  components: {
    NetworkView
  },
  data() {
    return {
      width: document.body.clientWidth,
      height: document.body.clientHeight,
    }
  },
  mounted() {
    window.addEventListener('resize', this.resize = e => {
      this.width = document.body.clientWidth
      this.height = document.body.clientHeight
    })
  },
  beforeDestroy() {
    window,removeEventListener('resize', this.resize)
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
