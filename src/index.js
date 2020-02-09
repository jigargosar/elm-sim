// const Module = require('./Main.elm')
const Module = require('./BoardEditor.elm')
// const Module = require('./ReactorSimulation.elm')
// require('tachyons')
require('./styles.css')

{
  const app = initElmApp()
  app.ports.getScrollbarSize.subscribe(function() {
    app.ports.gotScrollbarSize.send([
              window.innerWidth - document.body.clientWidth,
              window.innerHeight - document.body.clientHeight,
          ])
  })


}

function initElmApp() {
  // const app = Module.Elm.Main.init({
  const app = Module.Elm.BoardEditor.init({
  // const app = Module.Elm.ReactorSimulation.init({
    node: document.getElementById('root'),
    flags: {
      now: Date.now(),
      scrollbarSize: [
          window.innerWidth - document.body.clientWidth,
          window.innerHeight - document.body.clientHeight,
      ]
    },
  })
  return app
}

