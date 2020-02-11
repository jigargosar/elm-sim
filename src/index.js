const Module = require('./Main.elm')
require('./styles.css')
require('ramda')

{
  initElmApp()
}

function initElmApp() {
  const app = Module.Elm.Main.init({
    node: document.getElementById('root'),
    flags: {
      now: Date.now(),
      viewSize: [window.innerWidth, window.innerHeight],
      scrollbarSize: [
        window.innerWidth - document.body.clientWidth,
        window.innerHeight - document.body.clientHeight,
      ],
    },
  })
  return app
}
