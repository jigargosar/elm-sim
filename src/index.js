const Module = require('./Main.elm')
// require('tachyons')
// require('./styles.css')

{
  initElmApp()
}

function initElmApp() {
  const app = Module.Elm.Main.init({
    node: document.getElementById('root'),
    flags: {
      now: Date.now(),
    },
  })
  return app
}
