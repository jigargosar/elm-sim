const Module = require('./CanvasWalker.elm')
require('./styles.css')

Module.Elm.CanvasWalker.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})


console.log(Module)
