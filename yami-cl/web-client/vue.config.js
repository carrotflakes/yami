// See https://github.com/vuejs/vue-cli/issues/2948#issuecomment-438589725
module.exports = {
  chainWebpack: config => config.resolve.symlinks(false)
}
