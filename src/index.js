require('elm-canvas')
const Module = require('./GameOfLifeHtml.elm')
require('./styles.css')

Module.Elm.GameOfLifeHtml.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})


console.log(Module)
