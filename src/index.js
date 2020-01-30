const Module = require('./Main.elm')
require('tachyons')
require('./styles.css')
const dat = require('dat.gui')

// console.log(Module)
const gui = new dat.GUI()

const elmData = { zoom: 1 }

gui.add(elmData, 'zoom').step(0.5).min(0.1).max(50)

Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})
