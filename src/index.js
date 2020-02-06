const Module = require('./Main.elm')
// const Module = require('./ReactorSimulation.elm')
// require('tachyons')
require('./styles.css')

{
  initElmApp()
}

function initElmApp() {
  const app = Module.Elm.Main.init({
  // const app = Module.Elm.ReactorSimulation.init({
    node: document.getElementById('root'),
    flags: {
      now: Date.now(),
    },
  })
  return app
}
