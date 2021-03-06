// const Module = require('./Main.elm')
// const Module = require('./BoardEditor.elm')
const Module = require('./TodoistInterface.elm')
// const Module = require('./ReactorSimulation.elm')
// require('tachyons')
require('./styles.css')

{
  const app = initElmApp()
  // app.ports.getScrollbarSize.subscribe(function() {
  //   app.ports.gotScrollbarSize.send([
  //     window.innerWidth - document.body.clientWidth,
  //     window.innerHeight - document.body.clientHeight,
  //   ])
  // })
  window.addEventListener('resize', function() {
    // app.ports.gotViewSize.send([window.innerWidth, window.innerHeight])
  })

  app.ports.focusIdNextTick.subscribe(function(hid) {
    setTimeout(function() {
      const el = document.getElementById(hid)
      if (el) {
        el.focus()
      } else {
        console.warn('Focus Failed: NotFound ' + hid)
      }
    }, 0)
  })
}

function initElmApp() {
  // const app = Module.Elm.Main.init({
  const app = Module.Elm.TodoistInterface.init({
    // const app = Module.Elm.BoardEditor.init({
    // const app = Module.Elm.ReactorSimulation.init({
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
