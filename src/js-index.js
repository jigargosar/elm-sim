import { identity, isNil, mathMod } from 'ramda'

const canvasEl = document.getElementById('canvas-el')

const ctx = canvasEl.getContext('2d')

const gridConfig = { rowCount: 30, colCount: 30 }
const canvasSize = { width: ctx.canvas.width, height: ctx.canvas.height }
const cellSize = (canvasSize.width - 2) / gridConfig.colCount
const cellCount = gridConfig.rowCount * gridConfig.colCount

ctx.strokeStyle = 'black'
ctx.lineWidth = 1.5

function drawCell(x, y, cell) {
  ctx.beginPath()
  if (cell === 0) {
    ctx.fillStyle = 'yellow'
  } else {
    ctx.fillStyle = 'red'
  }

  ctx.rect(x * cellSize + 1, y * cellSize + 1, cellSize, cellSize)
  ctx.fill()
  ctx.stroke()
}

function randomCell() {
  return Math.random() < 0.4 ? 1 : 0
}

function renderGrid(grid) {
  grid.forEach((cell, i) => {
    const [x, y] = arrayIndexToXY(i)
    drawCell(x, y, cell)
  })
}

function arrayIndexToXY(i) {
  return [
    Math.round(i % gridConfig.colCount),
    Math.floor(i / gridConfig.rowCount),
  ]
}

function toggleCell(cell) {
  return cell === 0 ? 1 : 0
}

const neighbourIndices = Array.of(
  [
    [-1, -1],
    [-1, 0],
    [-1, 1],
  ],
  [
    [0, -1],
    /*ignore self (0,0)*/
    [0, 1],
  ],
  [
    [1, -1],
    [1, 0],
    [1, 1],
  ],
).flat(1)

console.log(neighbourIndices)

function aliveNeighboursCountOf(cx, cy, grid) {
  return neighbourIndices.reduce((ct, [x, y]) => {
    return getCellAt(cx + x, cy + y, grid) === 1 ? ct + 1 : ct
  }, 0)
}

function getCellAt(x_, y_, grid) {
  const [x, y] = [
    mathMod(x_, gridConfig.colCount),
    mathMod(y_, gridConfig.rowCount),
  ]

  const cell = grid[y * gridConfig.rowCount + x]
  return isNil(cell) ? 0 : cell
}

function nextCell(x, y, cell, grid) {
  const aliveCt = aliveNeighboursCountOf(x, y, grid)
  if (cell === 1) return aliveCt < 2 || aliveCt > 3 ? 0 : 1
  else {
    return aliveCt === 3 ? 1 : 0
  }
}

function nextGrid(grid) {
  return grid.map((cell, i) => {
    const [x, y] = arrayIndexToXY(i)

    return nextCell(x, y, cell, grid)
  })
}

function renderLoop(grid) {
  requestAnimationFrame(() => {
    renderGrid(grid)
    renderLoop(nextGrid(grid))
  })
}

const grid = new Array(cellCount).fill(0).map(randomCell)

renderLoop(grid)
