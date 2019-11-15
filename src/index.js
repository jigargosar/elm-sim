const Module = require('./Main.elm')

Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})
