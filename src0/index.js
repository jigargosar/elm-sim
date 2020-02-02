const R = require('ramda')

const Module = require('./Main.elm')
require('tachyons')
require('./styles.css')
const dat = require('dat.gui')

// console.log(Module)

{
  const app = initElmApp()
  initDatGUI(app)
}

function initDatGUI(elmApp) {
  const gui = new dat.GUI()

  const elmData = { zoom: 1 }
  const ports = Ports(elmApp)

  gui
    .add(elmData, 'zoom')
    .step(0.01)
    .min(0.1)
    .max(50)
    .onChange(function(value) {
      ports.send('onDatGUIChange', { [this.property]: value })
    })
    .onFinishChange(function(...args) {
      // console.log('zoomFinishChange', this, args)
    })
  gui.hide()
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

function Ports(elmApp) {
  return {
    send(subscriptionPortName, data) {
      const subscriptionPort = R.path(
        ['ports', subscriptionPortName, 'send'],
        elmApp,
      )
      if (R.isNil(subscriptionPort)) {
        console.warn(`PortNotFound: Ports.${subscriptionPortName}.send`)
      } else {
        subscriptionPort(data)
      }
    },
  }
}
