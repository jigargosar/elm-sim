const Module = require('./Main.elm')
require('tachyons')
require('./styles.css')

// console.log(Module)

Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})
