const canvasEl = document.getElementById('canvas-el')

const ctx = canvasEl.getContext('2d')

const gridConfig = { rowCount: 30, colCount: 30 }
const canvasSize = { width: ctx.canvas.width, height: ctx.canvas.height }
const cellSize = (canvasSize.width - 2) / gridConfig.colCount
const cellCount = gridConfig.rowCount * gridConfig.colCount

function drawCell(x, y, cell) {
  ctx.beginPath()
  if (cell === 0) {
    ctx.fillStyle = 'yellow'
  } else {
    ctx.fillStyle = 'red'
  }
  ctx.strokeStyle = 'black'
  ctx.lineWidth = 1.5
  ctx.rect(x * cellSize + 1, y * cellSize + 1, cellSize, cellSize)
  ctx.fill()
  ctx.stroke()
}

function randomCell() {
  return Math.random() < 0.1 ? 1 : 0
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

function nextCell(x, y, cell, grid) {
  return Math.random() < 0.01 ? toggleCell(cell) : cell
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
