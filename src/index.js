import { keys, pathOr } from 'ramda'

require('elm-canvas')
const Module = require('./GameOfLifeSvg.elm')
require('./styles.css')

initElmApp(Module, {
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})

function initElmApp(elmModule, initArgs) {
  const appNames = keys(pathOr({}, ['Elm'], elmModule))
  invariant(appNames.length === 1)
  return elmModule['Elm'][appNames[0]].init(initArgs)
}

function invariant(bool, msg = 'invariant failed') {
  if (!bool) {
    throw new Error(msg)
  }
}
