<template>
  <div id="app">
    <NetworkView/>
  </div>
</template>

<script>
import NetworkView from './components/NetworkView.vue'

export default {
  name: 'app',
  components: {
    NetworkView
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
