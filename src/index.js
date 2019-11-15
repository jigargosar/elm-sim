const Module = require('./Main.elm')
require('./styles.css')

Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})
